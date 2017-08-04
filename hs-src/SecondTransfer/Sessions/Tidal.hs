{-# LANGUAGE GADTs, TemplateHaskell, OverloadedStrings #-}
{- | A simple session manager that prunes the number of connections from time to time....
-}
module SecondTransfer.Sessions.Tidal (
       TidalContext                                     (..)
     , TidalCtxAction                                   (..)
     , TidalReporterCallback
     , TidalS
     , maxConnectionPerPeer_TiC
     , highWaterMark_TiC
     , maybeTidalReporter_TiC
     , newTidalSession
     , defaultTidalContext
     , tidalConnectionManager
     , closeAllConnections
     , signalConnectionForClose
       ) where


import           Control.Lens
import           Control.Monad.IO.Class                 (liftIO)
--import           Control.DeepSeq                        (
                                                         --($!!),
--                                                        deepseq )
import           Control.Monad.Trans.Reader
import           Control.Concurrent
import qualified Control.Exception                      as E
import           Control.Monad.ST
import           Control.Monad                          (
                                                          foldM
                                                        --, mapM_
                                                        , forM_
                                                        -- , mapM
                                                        , filterM
                                                        , when
                                                        )

--import qualified Data.ByteString                        as B
--import qualified Data.ByteString.Builder                as Bu
--import qualified Data.ByteString.Lazy                   as Bl
--import qualified Data.HashTable.IO                      as H
import qualified Data.Vector                            as DVec
--import qualified Data.Sequence                          as Sq
--import           Data.IORef
import qualified Data.HashTable.ST.Cuckoo               as Ht
import           Data.Maybe                             (catMaybes, isJust)
import           Data.Vector.Algorithms.Merge           (sortBy)
import qualified Data.IntSet                            as IntSet

import           System.Mem.Weak
import           System.Clock

import           SecondTransfer.Sessions.Config
import           SecondTransfer.IOCallbacks.Types



-- | Tell about tidal interventions using these
--   datums
data TidalCtxAction =
     HighWatermarkReached_TCA
    |DroppingExcessConnections_TCA Int
    deriving (Show, Eq)


-- | Invoked when there is something interesting to say.
type TidalReporterCallback =
    TidalCtxAction -> IO ()


-- | Configuration structure
data TidalContext = TidalContext {
    -- | Number of admissible connections per peer
    _maxConnectionPerPeer_TiC :: Int
    -- | Callback to be used when a connection to a peer is lost.
    -- | High water mark. When the number of connections go higher than this number,
    --   some of them are pruned.
  , _highWaterMark_TiC        :: Int
    -- | Callback that this manager will use to reporter interesting things happening
    --   in the manager.
  , _maybeTidalReporter_TiC   :: Maybe TidalReporterCallback
  }

makeLenses ''TidalContext


defaultTidalContext :: TidalContext
defaultTidalContext = TidalContext {
    _maxConnectionPerPeer_TiC = 8
  , _highWaterMark_TiC = 780
  , _maybeTidalReporter_TiC = Nothing
  }


-- | Contains information about each connection, and the eagerness we should
--   apply when deciding if we should close it.
data ConnectionEntry = ConnectionEntry {
  _hashableSockAddr_CE :: HashableSockAddr ,
  _handle_CE :: SessionGenericHandle
  }


makeLenses ''ConnectionEntry


type ConnectionList =  [MVar ConnectionEntry]


-- | State structure. Will live for the entire server lifetime
data TidalS = TidalS {
    _context_TdS        ::  TidalContext
  , _connections_TdS    ::  MVar ConnectionList
  -- | If a connection is added to this set, we drop it as soon as it
  --   becomes inactive, meaning it is inactive for five seconds or more.
  --   Notice that we have related timeouts in Config, but those
  --   apply to the situation when the server actively wants to close a
  --   connection that may be alive.
  , _dropOnInactive_TdS :: MVar IntSet.IntSet
  -- | If a connection is added to this set, we drop it urgently.
  , _dropNow_TdS        :: MVar IntSet.IntSet
    }


makeLenses ''TidalS


type TidalM = ReaderT TidalS IO


justRegisterNewConnection :: HashableSockAddr -> MVar bool -> SessionGenericHandle -> TidalM ()
justRegisterNewConnection sock_addr weakkey sgh =
  do
    connection_vector_mvar <- view connections_TdS
    liftIO $ do
        new_entry <- newMVar $
            ConnectionEntry {
               _hashableSockAddr_CE = sock_addr,
               _handle_CE = sgh
            }
        let
            free_action = do
                _ <- takeMVar new_entry
                return ()
        _ <- mkWeakMVar weakkey free_action
        modifyMVar_ connection_vector_mvar $ \ sq ->  do
                return $ new_entry : sq

type MemoTable s = Ht.HashTable s HashableSockAddr Int


-- | Prunes any connections over the limit of connections allowed per host.
pruneSameHost :: TidalM ()
pruneSameHost =
  do
    -- number below, it is per peer.
    max_connections <- view (context_TdS . maxConnectionPerPeer_TiC)
    maybe_reporter <- view (context_TdS . maybeTidalReporter_TiC)
    current_connections_mvar <- view connections_TdS
    to_drop <- liftIO . modifyMVar current_connections_mvar $ \ current_connections -> do
        let
            countAndAdvance :: MemoTable RealWorld -> HashableSockAddr -> IO Bool
            countAndAdvance h addr = stToIO $  do
                e <- Ht.lookup h addr
                case e of
                    Just n ->
                        if n >= max_connections
                          then
                            return False
                          else do
                            Ht.insert h addr (n + 1)
                            return True
                    Nothing ->
                        do
                          Ht.insert h addr 1
                          return True

            foldOperator :: MemoTable RealWorld -> ConnectionList -> MVar ConnectionEntry -> IO ConnectionList
            foldOperator h drop_connections entry =
              do
                maybe_entry <- tryReadMVar entry
                case maybe_entry of
                    Nothing -> return drop_connections
                    Just  ce  -> do
                        keep <- countAndAdvance  h (ce ^. hashableSockAddr_CE)
                        if keep
                            then return drop_connections
                            else return ( entry :drop_connections )

            -- A list of connections to drop
            to_drop :: IO ConnectionList
            to_drop =   do
                e <- stToIO $ Ht.new
                foldM (foldOperator e) [] current_connections

        -- Remove any old sessions from the list of live connections.
        remaining <- dropDeathConnections current_connections
        return (remaining, to_drop)

    -- We can invoke this function outside the lock, since it is not going to change the list .
    --
    -- to_drop_list is a list of connections to drop because the client host has too many
    -- sessions open.
    to_drop_list <- liftIO $ do to_drop
    -- The actual count of connections dropped, after accounting for connections that
    -- has already been closed.
    dropped_count <- liftIO $ dropConnections to_drop_list
    case maybe_reporter of
        Nothing -> return ()
        Just reporter -> liftIO $
            -- Only produce the watermark message if there were actually a few connections dropped
            -- here
            if  (dropped_count > 0)
              then do
                reporter HighWatermarkReached_TCA
                reporter (DroppingExcessConnections_TCA dropped_count)
              else do
                return ()



    return ()


--pruneNOldest


-- | Expects the connection list to be locked. Drops the death connections
--   and returns a list with the entries for the ones which are still alive.
dropDeathConnections :: ConnectionList -> IO ConnectionList
dropDeathConnections conns = filterM  (\ entry_mvar -> do
                                            maybe_entry <- tryReadMVar entry_mvar
                                            return $ isJust maybe_entry)  conns


dropConnections :: ConnectionList -> IO Int
dropConnections conns = foldM  (\ counter entry_mvar -> do
        maybesomething <- tryTakeMVar  entry_mvar
        case maybesomething of
            Nothing -> do
                return counter
            Just ce  -> do
                case ce ^. handle_CE  of
                    Whole_SGH a -> do
                        cleanlyCloseSession a
                        counter `seq` (return $ counter + 1)
    )
    0
    conns


-- | Takes extreme care (e.g, waits for it) when dropping the connections, so that the
--   user doesn't notice. We only use this when closing
--   ShimmerCat.
gentlyDropConnections :: ConnectionList -> IO Int
gentlyDropConnections conns = do
    waitable_mvars <- mapM  (\ entry_mvar -> do
        maybesomething <- tryTakeMVar  entry_mvar :: IO (Maybe ConnectionEntry)
        case maybesomething of
            Nothing -> do
                return Nothing
            Just ce  -> do
                case ce ^. handle_CE of
                    Whole_SGH a -> do
                        session_terminated <- newEmptyMVar
                        forkFinally
                          (do
                              cleanlyCloseSession a
                          )
                          (\ _either_exc_a -> do
                                 putMVar session_terminated ()
                                 case _either_exc_a of
                                     Left _ -> do
                                         putStrLn "Session-closing microthread at Tidal//gentlyDropConnections had an exception"
                                     Right _ -> return ()
                          )
                        return $ Just session_terminated
      )
      conns
    let
      connections_dropped = length waitable_mvars
      waitable_mvars' = catMaybes waitable_mvars
    -- Now just wait for the pending ones
    mapM takeMVar waitable_mvars'
    return connections_dropped


-- | Thread. Keeps closing any connections that land in _dropOnInactive_TdS and _dropNow_TDS
dropUndesirableConnections :: TidalM ()
dropUndesirableConnections =
  do
    connection_list_mvar <- view connections_TdS
    drop_on_inactive_mvar <- view dropOnInactive_TdS
    drop_on_inactive <- liftIO . readMVar $ drop_on_inactive_mvar
    drop_now_mvar <- view dropNow_TdS
    now <- liftIO . getTime $ Monotonic
    drop_now <- liftIO . readMVar $ drop_now_mvar
    waitable_mvars <- liftIO . withMVar connection_list_mvar $ \ conns ->  mapM  (\ entry_mvar -> do
        maybesomething <- tryTakeMVar  entry_mvar :: IO (Maybe ConnectionEntry)
        case maybesomething of
            Nothing -> do
                return Nothing
            Just ce  -> do
                case ce ^. handle_CE of
                    Whole_SGH a -> do
                        -- Get the connection id
                        let
                           (ConnectionId connection_id_int) = getConnectionId a
                           conn_id = fromIntegral connection_id_int
                        -- TO-DO: Make the drop_now a bit more violent
                        if (conn_id `IntSet.member` drop_now)
                          then
                            drop_connection a
                          else if (conn_id `IntSet.member` drop_on_inactive)
                            then do
                              -- Check if the connection is active or inactive.
                              session_last_activity <- liftIO . sessionLastActivity $ a
                              let
                                  time_inactive =
                                     toNanoSecs (now - session_last_activity) `div` 1000000000 :: Integer
                              if time_inactive > 5 -- <- Inactivity time right there!! MAKE A configuratble CONSTANT!!!
                                then
                                  drop_connection a
                                else do
                                  -- Restore the connection to the list
                                  liftIO $ putMVar entry_mvar ce
                                  return Nothing
                          else do
                            -- Restore the connection to the list
                            liftIO $ putMVar entry_mvar ce
                            return Nothing
      )
      conns
    let
      connections_dropped = length waitable_mvars
      waitable_mvars' = catMaybes waitable_mvars
    -- Now just wait for the pending ones
    liftIO $ do
        mapM takeMVar waitable_mvars'
        -- Wait five seconds more to invoke recursively
        threadDelay $ 5 * 1000 * 1000
    -- and call recursively
    dropUndesirableConnections
  where
    drop_connection a = do
      session_terminated <- newEmptyMVar
      forkFinally
        (do
            cleanlyCloseSession a
        )
        (\ _either_exc_a -> do
               putMVar session_terminated ()
               case _either_exc_a of
                   Left _ -> do
                       putStrLn "Session-closing microthread at Tidal//gentlyDropConnections had an exception"
                   Right _ -> return ()
        )
      return $ Just session_terminated



-- | Drops the oldest connections without activity ....
pruneOldestConnections :: Int -> TidalM ()
pruneOldestConnections how_many_to_drop =
  do
    -- First, create a vector with the information we are interested in ....
    current_connections_mvar <- view connections_TdS
    to_drop <- liftIO . withMVar current_connections_mvar $ \ current_connections -> do
        sortable_conns_list <- catMaybes <$> mapM (\ entry_mvar ->  do
                                                     w <- tryReadMVar entry_mvar
                                                     case w of
                                                         Just ce -> return . Just $  (ce ^. hashableSockAddr_CE, entry_mvar, ce ^. handle_CE)
                                                         Nothing -> return Nothing
                                               ) current_connections
        let
            sortable_vector = DVec.fromList $ sortable_conns_list
        with_time_spec <- DVec.mapM (\ (_addr, entry_mvar, gsh) -> do
                                       last_act_time <- sessionLastActivity gsh
                                       return ( entry_mvar, last_act_time)
                                 ) sortable_vector

        let
            -- Now the oldest ones in the activity vector are first.
            compare_with_spec ( _, t1) ( _, t2) = compare t1 t2
            sorted_time_spec = DVec.modify (\ mv -> sortBy compare_with_spec mv) with_time_spec

            -- This contains everybody that we can drop ...
            to_drop = DVec.take how_many_to_drop sorted_time_spec
        return . DVec.toList . DVec.map fst $ to_drop
    _ <- liftIO $ gentlyDropConnections to_drop
    return ()


-- | This simple callback is executed every time a new connection is opened.
--   The callback keeps a growing counter, but it doesn't track connections
--   that close naturally.
--   When the high watermark is reached, it does a cleanup by removing
--   any connections which has been closed and such.
whenAddingConnection ::  HashableSockAddr -> SessionGenericHandle -> MVar Bool -> TidalM ()
whenAddingConnection sock_addr handle already_closed_mvar =
  do
    highwater_mark <- view (context_TdS . highWaterMark_TiC)
    connections_mvar <- view connections_TdS
    connection_count <- liftIO . withMVar connections_mvar $ \ current_connections  -> do
        let
            connection_count = length current_connections
        return connection_count

    when (connection_count >= highwater_mark) $ do
        -- Take measures!
        pruneSameHost
        pruneOldestConnections (highwater_mark `div` 3)

    justRegisterNewConnection sock_addr already_closed_mvar handle


newTidalSession :: TidalContext -> IO TidalS
newTidalSession tidal_context = do
    connections <- newMVar []
    drop_on_inactive <- newMVar IntSet.empty
    drop_now <- newMVar IntSet.empty
    return TidalS {
      _context_TdS = tidal_context,
      _connections_TdS = connections,
      _dropOnInactive_TdS = drop_on_inactive,
      _dropNow_TdS = drop_now
      }


tidalConnectionManager :: TidalS -> NewSessionCallback
tidalConnectionManager tidals   =
    NewSessionCallback $ \ a b c -> runReaderT (whenAddingConnection a b c) tidals


closeAllConnections :: TidalS -> IO ()
closeAllConnections
    (TidalS { _context_TdS = _tidal_context, _connections_TdS = connections_mvar }) = do
    withMVar connections_mvar $ \ connections -> do
        _ <- gentlyDropConnections connections
        return ()


signalConnectionForClose :: TidalS -> ConnectionId -> EagernessToDrop -> IO ()
signalConnectionForClose tidals (ConnectionId connection_id_int) DropItNow_ETD = do
    modifyMVar_ (tidals ^. dropNow_TdS) $ \ old_set ->
        return $ IntSet.insert  (fromIntegral connection_id_int) old_set
signalConnectionForClose tidals (ConnectionId connection_id_int) DropIt_ETD = do
    modifyMVar_ (tidals ^. dropOnInactive_TdS) $ \ old_set ->
        return $ IntSet.insert  (fromIntegral connection_id_int) old_set
signalConnectionForClose tidals (ConnectionId connection_id_int) _ = return ()
