{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http2.MakeAttendant (
    http2Attendant
    ) where


import           SecondTransfer.Http2.Framer            (wrapSession, SessionPayload(..))
import           SecondTransfer.Sessions.Internal       (SessionsContext)
import           SecondTransfer.MainLoop.CoherentWorker

import           SecondTransfer.IOCallbacks.Types       (Attendant)

-- |
--
-- @
--      http2Attendant :: AwareWorker -> AttendantCallbacks ->  IO ()
-- @
--
-- Given a `AwareWorker`, this function wraps it with flow control, multiplexing,
-- and state maintenance needed to run an HTTP/2 session.
--
-- Notice that this function is  using HTTP/2 over TLS.
http2Attendant :: SessionsContext -> AwareWorker -> Attendant
http2Attendant sessions_context coherent_worker attendant_callbacks = do
    let
        attendant = wrapSession (AwareWorker_SP coherent_worker) sessions_context
    attendant attendant_callbacks
