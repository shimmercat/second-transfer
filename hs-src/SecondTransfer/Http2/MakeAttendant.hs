{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http2.MakeAttendant (
    http2Attendant
    ) where


import           SecondTransfer.Http2.Framer            (wrapSession)
import           SecondTransfer.Sessions.Internal       (SessionsContext)
import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.MainLoop.PushPullType   (
														 --CloseAction,
                                                         --PullAction, 
                                                         --PushAction,
                                                         Attendant
                                                         )

-- | The type of this function is equivalent to:
--  
-- @      
--      http2Attendant :: CoherentWorker -> PushAction -> PullAction -> CloseAction ->  IO ()
-- @
-- 
-- Given a `CoherentWorker`, this function wraps it with flow control, multiplexing,
-- and state maintenance needed to run an HTTP/2 session. 
--
-- Notice that this function is  using HTTP/2 over TLS. We haven't implemented yet
-- a session handling mechanism for HTTP/1.1 . 
http2Attendant :: SessionsContext -> CoherentWorker -> Attendant
http2Attendant sessions_context coherent_worker push_action pull_action  close_action = do 
    let 
        attendant = wrapSession coherent_worker sessions_context
    attendant push_action pull_action close_action    