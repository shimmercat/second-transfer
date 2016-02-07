{-# LANGUAGE ExistentialQuantification, TemplateHaskell, DeriveDataTypeable, Rank2Types, OverloadedStrings #-}
module SecondTransfer.IOCallbacks.Coupling (
                 Coupling
               , couple
               , breakCoupling
               , sendSourceToIO
               , iocallbacksToSink
               , popIOCallbacksIntoExistance
               , IOCSideA
               , IOCSideB
       )where

import           Control.Lens
import           Control.Concurrent
import           Control.Monad.IO.Class                       (liftIO, MonadIO)
import qualified Control.Exception                            as E

import           Data.IORef
--import           Data.Typeable
import           Data.Conduit
import qualified Data.Conduit.List                            as DCL

import qualified Data.ByteString                              as B
import qualified Data.ByteString.Lazy                         as LB
import qualified Data.ByteString.Builder                      as Bu


import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.Exception                     (IOProblem, NoMoreDataException(..), forkIOExc)


-- | A coupling between two IOCallbacks. It is breakable...
data Coupling = Coupling {
    _breakNow_Cou :: MVar ()
    }

makeLenses ''Coupling

-- TODO: Consider if we must report errors! how do we do that? Right now I'll re-throw exceptions,
--       but since this runs in its own thread, nobody will notice. A better coping strategy would
--       be to perhaps store the exception somewhere.
pump :: String -> MVar () -> BestEffortPullAction -> PushAction -> IO ()
pump tag break_now pull push = do
    let
        go = do
            must_finish <- tryTakeMVar break_now
            case must_finish of
                Nothing -> do
                    either_datum <-  E.try (pull True) :: IO (Either IOProblem B.ByteString)
                    must_finish' <- tryTakeMVar break_now
                    case (must_finish', either_datum) of

                        (Nothing, Right datum) -> do
                            either_ok <- E.try (push . LB.fromStrict $ datum) :: IO (Either IOProblem () )
                            case either_ok of
                                Right _ ->  go

                                Left _e -> do
                                    _succeeded <- tryPutMVar break_now ()
                                    return ()

                        (Just (), _) ->
                            return ()

                        (Nothing, Left _e ) -> do
                            _succeeded <- tryPutMVar break_now ()
                            return ()

                Just _ ->
                    return ()
    go


-- | Connects two IO callbacks so that data received in one is sent to the
--   other.
couple :: IOCallbacks -> IOCallbacks -> IO Coupling
couple a b =
  do
    break_now_mvar <- newEmptyMVar

    _pump_thread1 <- forkIOExc "pump1" $ pump "1to2" break_now_mvar ( a ^. bestEffortPullAction_IOC ) ( b ^. pushAction_IOC)
    _pump_thread2 <- forkIOExc "pump2 "$ pump "2to1" break_now_mvar ( b ^. bestEffortPullAction_IOC ) ( a ^. pushAction_IOC)

    return Coupling {
        _breakNow_Cou = break_now_mvar
        }


breakCoupling :: Coupling -> IO ()
breakCoupling coupling = do
    _succeeded <- tryPutMVar (coupling ^. breakNow_Cou ) ()
    return ()


iocallbacksToSink :: MonadIO m => IOCallbacks -> Sink LB.ByteString m ()
iocallbacksToSink ioc = DCL.mapM_ (
    \ s -> do
         --putStrLn $ "send: " ++ (show s)
         liftIO $ ioc ^. pushAction_IOC $ s
    )


-- | Sends the data coming from the source to the IOCallbacks.
-- No exceptions are handled here. This consumes the thread until
-- it finishes. The iocallbacks is not closed.
sendSourceToIO :: MonadIO m => Source m LB.ByteString -> IOCallbacks -> m ()
sendSourceToIO source ioc =
  source $$ iocallbacksToSink ioc

data SideDatum = SideDatum {
     _dt_SD :: IORef Bu.Builder
   , _dt_M  :: MVar ()
     }

newSideDatum :: IO SideDatum
newSideDatum =
  do
    r <- newIORef mempty
    m <- newEmptyMVar
    return SideDatum { _dt_SD = r , _dt_M = m}

makeLenses ''SideDatum

data IOCTransit = IOCTransit {
    _aToB_IOCT   :: SideDatum
  , _bToA_IOCT   :: SideDatum
  , _closed_IOCT :: MVar ()
    }

makeLenses ''IOCTransit

newtype IOCSideA = IOCSideA (IOCTransit, IOCallbacks)

newtype IOCSideB = IOCSideB (IOCTransit, IOCallbacks)


popIOCallbacksIntoExistance :: IO (IOCSideA, IOCSideB)
popIOCallbacksIntoExistance = do
    sd1 <- newSideDatum
    sd2 <- newSideDatum
    closed <- newEmptyMVar
    let
        t = IOCTransit {
            _aToB_IOCT = sd1
          , _bToA_IOCT = sd2
          , _closed_IOCT = closed
            }
    let
        action_set_a = popActions t aToB_IOCT bToA_IOCT
        action_set_b = popActions t bToA_IOCT aToB_IOCT
    return
        (
          IOCSideA (t, action_set_a),
          IOCSideB (t, action_set_b)
        )

popActions :: IOCTransit -> Lens' IOCTransit SideDatum -> Lens' IOCTransit SideDatum -> IOCallbacks
popActions t pullside pushside =
  let

    throwIfNeeded :: IO ()
    throwIfNeeded = do
        m <- tryReadMVar (t ^. closed_IOCT)
        case m of
            Nothing  -> return ()
            Just _ -> E.throwIO NoMoreDataException

    push_action :: LB.ByteString -> IO ()
    push_action datum = do
        --putStrLn . show $ "PUSHED" `mappend` datum
        throwIfNeeded
        atomicModifyIORef'  (t ^. pushside . dt_SD) $ \ bu ->  (bu `mappend` (Bu.lazyByteString datum), () )
        -- Notify the other side that there is more data
        _ <- tryPutMVar (t ^. pushside . dt_M) ()
        return ()

    best_effort_pull_action can_block
        | can_block = do
            throwIfNeeded
            hath_data <- atomicModifyIORef' ( t ^. pullside . dt_SD ) $ \ cnt -> (mempty, cnt)
            let
                hath_data_bs  = Bu.toLazyByteString hath_data
            if LB.length hath_data_bs > 0
              then
                return $ LB.toStrict hath_data_bs
              else do
                _ <- takeMVar (t ^. pullside . dt_M)
                best_effort_pull_action True
       | otherwise = do
            throwIfNeeded
            hath_data <- atomicModifyIORef' ( t ^. pullside . dt_SD ) $ \ cnt -> (mempty, cnt)
            let
                hath_data_bs  = LB.toStrict . Bu.toLazyByteString $ hath_data
            return hath_data_bs

    pull_action n = (LB.toStrict . Bu.toLazyByteString ) <$>  (pull_action' n mempty 0)

    pull_action' :: Int -> Bu.Builder -> Int -> IO Bu.Builder
    pull_action' asked bu nhath = do
        throwIfNeeded
        hath_data <- atomicModifyIORef' ( t ^. pullside . dt_SD ) $ \ cnt -> (mempty, cnt)
        let
            hath_data_bs =  Bu.toLazyByteString hath_data
        case fromIntegral $ LB.length hath_data_bs of
            ln | (ln + nhath) < asked  -> do
                let
                    nbu = bu `mappend` hath_data
                _ <- takeMVar (t ^. pullside . dt_M)
                pull_action' asked nbu (ln + nhath)

               | (ln + nhath) == asked ->
                    return $ bu `mappend` hath_data

               | (ln + nhath) > asked -> do
                    let
                        complete_thing = bu  `mappend` hath_data
                        (ret, keep) = LB.splitAt (fromIntegral asked) . Bu.toLazyByteString $ complete_thing
                    atomicModifyIORef'  ( t ^. pullside . dt_SD ) $ \ cnt -> ( Bu.lazyByteString keep `mappend` cnt, ())
                    _ <- tryPutMVar ( t ^. pullside . dt_M ) ()
                    return . Bu.lazyByteString $ ret

               | otherwise -> error "Function supposed to be total"

    close_action = do
        _ <- tryPutMVar (t ^. closed_IOCT ) ()
        return ()

    in IOCallbacks {
       _pushAction_IOC = push_action
     , _pullAction_IOC = pull_action
     , _bestEffortPullAction_IOC = best_effort_pull_action
     , _closeAction_IOC = close_action
       }


instance IOChannels IOCSideA where
    handshake (IOCSideA (_t,ioc)) = return ioc

instance IOChannels IOCSideB where
    handshake (IOCSideB (_t,ioc)) = return ioc
