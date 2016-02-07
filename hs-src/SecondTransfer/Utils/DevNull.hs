{-# LANGUAGE OverloadedStrings  #-}
module SecondTransfer.Utils.DevNull(
       dropIncomingData
    ,  dropWouldGoData
     ) where


import           Data.Conduit

import           Control.Monad.IO.Class                 (liftIO, MonadIO)
import qualified Control.Monad.Trans.Resource            as ReT

import SecondTransfer.MainLoop.CoherentWorker

import SecondTransfer.Exception                       (forkIOExc)

-- TODO: Handling unnecessary data should be done in some other, less
-- harmfull way... need to think about that.

-- | If you are not processing the potential POST input in a request,
-- use this consumer to drop the data to oblivion. Otherwise it will
-- remain in an internal queue until the client closes the
-- stream, and if the client doesn't want to do so....
dropIncomingData :: MonadIO m => Maybe InputDataStream -> m ()
dropIncomingData Nothing = return ()
dropIncomingData (Just data_source) = do
    _ <- liftIO . forkIOExc "dropIncomingData" . ReT.runResourceT $
         data_source $$ awaitForever (\ _ -> do
                                         return () )
    return ()


dropWouldGoData :: DataAndConclusion -> IO ()
dropWouldGoData data_source = do
    let
        do_empty = do
            x <- await
            case x of
                Nothing -> return ()
                Just _ -> do_empty
    _ <- forkIOExc "dropWouldGoData" $ do
        _ <- ReT.runResourceT . runConduit $ fuseBoth data_source  do_empty
        return ()
    return ()
