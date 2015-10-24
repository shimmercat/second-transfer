{-# LANGUAGE ExistentialQuantification, TemplateHaskell, DeriveDataTypeable #-}
module SecondTransfer.IOCallbacks.Coupling (
                 Coupling
               , couple
               , breakCoupling
       )where

import           Control.Lens
import           Control.Concurrent
--import           Control.Concurrent.MVar
import qualified Control.Exception                            as E

--import           Data.IORef
--import           Data.Typeable

import qualified Data.ByteString                              as B
import qualified Data.ByteString.Lazy                         as LB
--import qualified Data.ByteString.Builder                      as Bu


import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.Exception                     (IOProblem)


-- | A coupling between two IOCallbacks. It is breakable...
data Coupling = Coupling {
    _breakNow_Cou :: MVar ()
    }

makeLenses ''Coupling

-- TODO: Consider if we must report errors! how do we do that? Right now I'll re-throw exceptions,
--       but since this runs in its own thread, nobody will notice. A better coping strategy would
--       be to perhaps store the exception somewhere.
pump :: MVar () -> BestEffortPullAction -> PushAction -> IO ()
pump break_now pull push = do
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

                                Left e -> do
                                    tryPutMVar break_now ()
                                    E.throwIO e

                        (Just (), _) ->
                            return ()

                        (Nothing, Left e ) -> do
                            tryPutMVar break_now ()
                            E.throwIO e

                Just _ ->
                    return ()
    go



couple :: IOCallbacks -> IOCallbacks -> IO Coupling
couple a b = do
    break_now_mvar <- newEmptyMVar

    forkIO $ pump break_now_mvar ( a ^. bestEffortPullAction_IOC ) ( b ^. pushAction_IOC)
    forkIO $ pump break_now_mvar ( b ^. bestEffortPullAction_IOC ) ( a ^. pushAction_IOC)

    return Coupling {
        _breakNow_Cou = break_now_mvar
        }


breakCoupling :: Coupling -> IO ()
breakCoupling coupling = do
    tryPutMVar (coupling ^. breakNow_Cou ) ()
    return ()