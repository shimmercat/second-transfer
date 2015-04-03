{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http2.MakeAttendant (
    http2Attendant
    ) where


import           SecondTransfer.Http2.Framer            (wrapSession)
import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.MainLoop.PushPullType   (
														 --CloseAction,
                                                         --PullAction, 
                                                         --PushAction,
                                                         Attendant
                                                         )

-- | Equivalent to:
--  
-- @      
--      http2Attendant :: CoherentWorker -> PushAction -> PullAction -> CloseAction ->  IO ()
-- @
--      
http2Attendant :: CoherentWorker -> Attendant
http2Attendant coherent_worker push_action pull_action  close_action = do 
    let 
        attendant = wrapSession coherent_worker
    attendant push_action pull_action close_action    