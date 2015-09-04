{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http2.MakeAttendant (
    http2Attendant
    ) where


import           SecondTransfer.Http2.Framer            (wrapSession)
import           SecondTransfer.Sessions.Internal       (SessionsContext)
import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.MainLoop.PushPullType   (Attendant)

-- |
-- 
-- Given an `AwareWorker`, this function wraps it with flow control, multiplexing,
-- and state maintenance needed to run an HTTP/2 session.
--
-- Notice that this function is  using HTTP/2 over TLS. There is an equivalent for HTTP 
-- 1.1. 
http2Attendant :: SessionsContext -> AwareWorker -> Attendant
http2Attendant sessions_context coherent_worker attendant_callbacks = do
    let
        attendant = wrapSession coherent_worker sessions_context
    attendant attendant_callbacks
