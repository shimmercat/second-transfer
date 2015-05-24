
module SecondTransfer.Utils.DevNull(
    dropIncomingData
    ) where 

import SecondTransfer.MainLoop.CoherentWorker 
import Data.Conduit 


-- | If you are not processing the potential POST input in a request,
-- use this consumer to drop the data to oblivion. Otherwise it will 
-- remain in an internal queue until until the client closes the 
-- stream, and if the client doesn't want to do so....
dropIncomingData :: Maybe InputDataStream -> IO ()
dropIncomingData Nothing = return ()
dropIncomingData (Just data_source) = 
    data_source $$ (awaitForever (\ x -> return () ) )