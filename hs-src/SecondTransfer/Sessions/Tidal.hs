{-# LANGUAGE GADTs, TemplateHaskell, OverloadedStrings #-}
{- | A simple session manager that prunes the number of connections from time to time....
-}
module SecondTransfer.Sessions.Tidal (
       TidalContext                                     (..)
     , maxConnectionPerPeer_TiC
     , highWaterMark_TiC
     , newTidalSession
     , defaultTidalContext
     , tidalConnectionManager
       ) where


import           Control.Lens
import           Control.Monad.IO.Class                 (liftIO)
--import           Control.DeepSeq                        (
                                                         --($!!),
--                                                        deepseq )
import           Control.Monad.Trans.Reader
import           Control.Concurrent
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
import           Data.Maybe                             (catMaybes)
import           Data.Vector.Algorithms.Merge           (sortBy)

import           System.Mem.Weak
--import           System.Clock                           (TimeSpec)

import           SecondTransfer.Sessions.Config
import           SecondTransfer.IOCallbacks.Types


-- | Configuration structure
data TidalContext = TidalContext {
    -- | Number of admissible connections per peer
    _maxConnectionPerPeer_TiC :: Int
    -- | Callback to be used when a connection to a peer is lost.
    -- | High water mark. When the number of connections go higher than this number,
    --   some of them are pruned.
  , _highWaterMark_TiC        :: Int
  }

makeLenses ''TidalContext


defaultTidalContext :: TidalContext
defaultTidalContext = TidalContext {
    _maxConnectionPerPeer_TiC = 8
  , _highWaterMark_TiC = 256
  }

type ConnectionEntry = (HashableSockAddr ,Weak SessionGenericHandle)

type ConnectionList =  [ConnectionEntry]

-- | State structure. Will live for the entire server lifetime
data TidalS = TidalS {
    _context_TdS      ::  TidalContext
  , _connections_TdS  ::  MVar ConnectionList
    }

makeLenses ''TidalS


type TidalM = ReaderT TidalS IO


justRegisterNewConnection :: HashableSockAddr -> a -> SessionGenericHandle -> TidalM ()
justRegisterNewConnection sock_addr weakkey sgh =
  do
    connection_vector_mvar <- view connections_TdS
    liftIO $ do
        weakling <- mkWeak weakkey sgh Nothing
        modifyMVar_ connection_vector_mvar
            $ \ sq ->  return $ (sock_addr, weakling) : sq

type MemoTable s = Ht.HashTable s HashableSockAddr Int


-- | Prunes any connections over the limit of connections allowed per host.
pruneSameHost :: TidalM ()
pruneSameHost =
  do
    -- number below, it is per peer.
    max_connections <- view (context_TdS . maxConnectionPerPeer_TiC)
    current_connections_mvar <- view connections_TdS
    to_drop <- liftIO . modifyMVar current_connections_mvar $ \ current_connections -> do
        let
            countAndAdvance :: MemoTable s -> HashableSockAddr -> ST s Bool
            countAndAdvance h addr = do
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

            foldOperator :: MemoTable s -> ConnectionList -> ConnectionEntry -> ST s ConnectionList
            foldOperator h drop_connections (addr, weakling) =
              do
                keep <- countAndAdvance  h addr
                if keep
                    then return drop_connections
                    else return ( (addr,weakling):drop_connections )

            to_drop :: ConnectionList
            to_drop = runST $ do
                e <- Ht.new
                foldM (foldOperator e) [] current_connections

        remaining <- dropDeathConnections current_connections
        return (remaining, to_drop)

    -- We can invoke this function outside the lock, since it is not going to change the list .
    liftIO $ dropConnections to_drop

    return ()


--pruneNOldest


-- | Expects the connection list to be locked. Drops the death connections
--   and returns a list with the entries for the ones which are still alive.
dropDeathConnections :: ConnectionList -> IO ConnectionList
dropDeathConnections conns = filterM  (\ (_addr, weakling) -> do
    maybesomething <- deRefWeak weakling
    case maybesomething of
        Nothing -> return False
        _ -> return True
    ) conns


dropConnections :: ConnectionList -> IO ()
dropConnections conns = forM_  conns $ \ (_addr, weakling) -> do
    maybesomething <- deRefWeak weakling
    case maybesomething of
        Nothing -> return ()
        (Just generic_handle ) -> do
            case generic_handle of
                Whole_SGH a ->
                    cleanlyCloseSession a

                Partial_SGH _ iocallbacks ->
                    (iocallbacks ^. closeAction_IOC)


-- | Drops the oldest connections without activity ....
pruneOldestConnections :: Int -> TidalM ()
pruneOldestConnections how_many_to_drop =
  do
    -- First, create a vector with the information we are interested in ....
    current_connections_mvar <- view connections_TdS
    to_drop <- liftIO . withMVar current_connections_mvar $ \ current_connections -> do
        sortable_conns_list <- catMaybes <$> mapM (\ (addr, weakling) ->  do
                                                     w <- deRefWeak weakling
                                                     case w of
                                                         Just g -> return . Just $  (addr, g, weakling)
                                                         Nothing -> return Nothing
                                               ) current_connections
        let
            sortable_vector = DVec.fromList $ sortable_conns_list
        with_time_spec <- DVec.mapM (\ (addr, generic_handle, weakling) -> do
                                       last_act_time <- sessionLastActivity generic_handle
                                       return ( (addr, weakling), last_act_time)
                                 ) sortable_vector

        let
            -- Now the oldest ones in the activity vector are first.
            compare_with_spec ( _, t1) ( _, t2) = compare t1 t2
            sorted_time_spec = DVec.modify (\ mv -> sortBy compare_with_spec mv) with_time_spec

            -- This contains everybody that we can drop ...
            to_drop = DVec.take how_many_to_drop sorted_time_spec
        return . DVec.toList . DVec.map fst $ to_drop
    liftIO $ dropConnections to_drop


whenAddingConnection ::  HashableSockAddr -> SessionGenericHandle -> a -> TidalM ()
whenAddingConnection sock_addr handle key =
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
    -- And then, of course add the connection
    -- Maybe that behaviour needs to be changed
    justRegisterNewConnection sock_addr key handle


newTidalSession :: TidalContext -> IO TidalS
newTidalSession tidal_context = do
    connections <- newMVar []
    return TidalS { _context_TdS = tidal_context, _connections_TdS = connections }


tidalConnectionManager :: TidalS -> NewSessionCallback
tidalConnectionManager tidals a b c  =
    runReaderT (whenAddingConnection a b c) tidals
