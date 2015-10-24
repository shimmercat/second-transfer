{-# LANGUAGE OverloadedStrings  #-}
module SecondTransfer.Utils.DevNull(
    dropIncomingData
    ) where


import Control.Concurrent (forkIO)
import Data.Conduit

import SecondTransfer.MainLoop.CoherentWorker



-- TODO: Handling unnecessary data should be done in some other, less
-- harmfull way... need to think about that.

-- | If you are not processing the potential POST input in a request,
-- use this consumer to drop the data to oblivion. Otherwise it will
-- remain in an internal queue until the client closes the
-- stream, and if the client doesn't want to do so....
dropIncomingData :: Maybe InputDataStream -> IO ()
dropIncomingData Nothing = return ()
dropIncomingData (Just data_source) = do
    forkIO $
        data_source $$ awaitForever (\ _ -> do
                                         return () )
    return ()
