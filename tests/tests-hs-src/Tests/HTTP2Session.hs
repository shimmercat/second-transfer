{-# LANGUAGE OverloadedStrings #-}
module Tests.HTTP2Session where


import           Data.Typeable

import           Control.Concurrent               (threadDelay)
import qualified Control.Concurrent               as C (yield)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import qualified Network.HTTP2                    as NH2
import           Test.HUnit
import           SecondTransfer.Exception
import           SecondTransfer.Http2             (http2Attendant)
import           SecondTransfer.Sessions
import           SecondTransfer.Test.DecoySession
import           SecondTransfer.Types
import           SecondTransfer.Utils.HTTPHeaders (fetchHeader)
import           SecondTransfer.MainLoop.CoherentWorker (defaultEffects)
import           SecondTransfer.MainLoop.ClientPetitioner



import           Data.Conduit                     (yield)



saysHello :: DataAndConclusion
saysHello = do
    yield "Hello world!\ns"
    -- No footers
    return []


simpleWorker :: AwareWorker
simpleWorker = coherentToAwareWorker . const $ return (
    [
        (":status", "200")
    ],
    [], -- No pushed streams
    saysHello
    )

data Internal500Exception = Internal500Exception
    deriving (Typeable, Show)

instance Exception Internal500Exception where
    toException = convertHTTP500PrecursorExceptionToException
    fromException = getHTTP500PrecursorExceptionFromException

throwingWorker :: AwareWorker
throwingWorker _ = throwIO Internal500Exception

throwingWorker2 :: AwareWorker
throwingWorker2  = coherentToAwareWorker . const .  return $ (
        [
            -- These headers will be already sent by the time
            -- the exception is discovered...
            (":status", "200")
        ],
        [], -- No pushed streams
        do
            yield "Error coming down"
            liftIO $ throwIO Internal500Exception
            return []
    )


simpleRequestHeaders :: Headers
simpleRequestHeaders = [
    (":path", "/"),
    (":authority", "www.example.com"),
    (":scheme", "https"),
    (":method", "GET")
    ]


badRequestHeaders :: Headers
badRequestHeaders = [
    (":path", "/"),
    (":authority", "www.example.com"),
    (":scheme", "https")
    ]


setError :: MVar Bool -> ErrorCallback
setError mvar = const $ modifyMVar_ mvar (const $ return True )


errorsSessionConfig :: MVar Bool -> SessionsConfig
errorsSessionConfig mvar = set (sessionsCallbacks . reportErrorCallback_SC)
    (Just $ setError mvar) defaultSessionsConfig


testPrefaceChecks :: Test
testPrefaceChecks = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context simpleWorker
    decoy_session <- createDecoySession attendant
    -- This should work
    sendRawDataToSession decoy_session "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
    threadDelay 1000000
    got_error <- readMVar errors_mvar
    if got_error then
        assertFailure "Exception raised"
    else
        return ()


testPrefaceChecks2 :: Test
testPrefaceChecks2 = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context simpleWorker
    decoy_session <- createDecoySession attendant
    -- This should work
    sendRawDataToSession decoy_session "PRI * HXXP/2.0\r\n\r\nSM\r\n\r\n"
    threadDelay 1000000
    got_error <- readMVar errors_mvar
    if not got_error then do
        assertFailure "Exception didn't raise"
    else
        return ()


testFirstFrameMustBeSettings :: Test
testFirstFrameMustBeSettings = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context simpleWorker
    decoy_session <- createDecoySession attendant
    sendRawDataToSession decoy_session "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
    maybe_frame <- recvFrameFromSession decoy_session
    case maybe_frame of
        Nothing ->
            assertFailure "Waiting a frame, received none"
        Just (NH2.Frame _ (NH2.SettingsFrame _)) -> -- Ok
            return ()

        _ ->
            assertFailure "Waiting a settings frame, received something else"


testFirstFrameMustBeSettings2 :: Test
testFirstFrameMustBeSettings2 = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context simpleWorker
    decoy_session <- createDecoySession attendant
    sendRawDataToSession decoy_session "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
    maybe_frame <- recvFrameFromSession decoy_session
    case maybe_frame of
        Nothing ->
            assertFailure "Waiting a frame, received none"
        Just (NH2.Frame _ (NH2.SettingsFrame _)) -> -- Ok
            return ()

        _ ->
            assertFailure "Waiting a settings frame, received something else"

    -- Send a settings frame now
    sendFrameToSession
        decoy_session
        ( (NH2.EncodeInfo NH2.defaultFlags 0 Nothing),
          (NH2.SettingsFrame [])
        )

    -- Check session is alive
    got_error <- readMVar errors_mvar
    if got_error then do
        assertFailure "Exception raised unexpectedly"
    else
        return ()

testFirstFrameMustBeSettings3 :: Test
testFirstFrameMustBeSettings3 = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context simpleWorker
    decoy_session <- createDecoySession attendant
    sendRawDataToSession decoy_session "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

    let d2 = ( (NH2.EncodeInfo NH2.defaultFlags 0 Nothing),
          (NH2.PingFrame "01234567") )

    -- Send a ping frame now, so that we get an error
    sendFrameToSession
        decoy_session
        d2

    -- We need to give some time to the framework to react to problems
    threadDelay 20000

    -- Check session is alive
    got_error <- readMVar errors_mvar
    if not got_error then do
        assertFailure "Exception didn't raise properly"
    else
        return ()


testIGet500Status :: Test
testIGet500Status = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context throwingWorker
    decoy_session <- createDecoySession attendant

    -- Send the prologue
    sendRawDataToSession decoy_session "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

    -- Send a settings frame now, otherwise the session will bark....
    sendFrameToSession
        decoy_session
        ( (NH2.EncodeInfo NH2.defaultFlags 0 Nothing),
          (NH2.SettingsFrame []) )

    -- Now perform a simple, mocking request
    performRequestSimple decoy_session 1 [
        (":method", "get"),
        (":scheme", "https"),
        (":authority", "www.example.com"),
        (":path", "/hi")
        ]

    -- Now we read a few frames
    seen <- return False
    f0 <- recvFrameFromSession decoy_session
    seen1 <- frameIsStatus500 decoy_session seen f0

    f1 <- recvFrameFromSession decoy_session
    seen2 <- frameIsStatus500 decoy_session seen f1

    f2 <- recvFrameFromSession decoy_session
    seen3 <- frameIsStatus500 decoy_session seen f2

    if not seen3 then do
        assertFailure "Didn't see that 500"
    else
        return ()


frameIsStatus500 :: DecoySession -> Bool -> Maybe NH2.Frame -> IO Bool
frameIsStatus500 decoy_session prev maybe_frame =
    case prev of
        True -> return True
        False ->
            case maybe_frame of
                Just (NH2.Frame _  (NH2.HeadersFrame _ bs ) ) -> do
                    headers <- decodeHeadersForSession decoy_session bs
                    let
                        maybe_status = fetchHeader headers ":status"
                    case maybe_status of
                        Just x | x == "500" -> return True
                        _                   -> return False

                _ ->
                    return False


testSessionBreaksOnLateError :: Test
testSessionBreaksOnLateError = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context throwingWorker2
    decoy_session <- createDecoySession attendant

    -- Send the prologue
    sendRawDataToSession decoy_session "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

    -- Send a settings frame now, otherwise the session will bark....
    sendFrameToSession
        decoy_session
        ( (NH2.EncodeInfo NH2.defaultFlags 0 Nothing),
          (NH2.SettingsFrame []) )

    -- Now perform a simple, mocking request
    performRequestSimple decoy_session 1 [
        (":method", "get"),
        (":scheme", "https"),
        (":authority", "www.example.com"),
        (":path", "/hi")
        ]

    -- Now we read a few frames
    seen <- return False
    f0 <- recvFrameFromSession decoy_session
    seen1 <- frameIsGoAwayBecauseInternalError decoy_session seen f0

    f1 <- recvFrameFromSession decoy_session
    seen2 <- frameIsGoAwayBecauseInternalError decoy_session seen1 f1

    f2 <- recvFrameFromSession decoy_session
    seen3 <- frameIsGoAwayBecauseInternalError decoy_session seen2 f2

    f3 <- recvFrameFromSession decoy_session
    seen4 <- frameIsGoAwayBecauseInternalError decoy_session seen3 f3

    if not seen4 then do
        assertFailure "Didn't see GoAwayFrame"
    else
        return ()


frameIsGoAwayBecauseInternalError :: DecoySession -> Bool -> Maybe NH2.Frame -> IO Bool
frameIsGoAwayBecauseInternalError decoy_session prev maybe_frame = do
    case prev of
        True -> return True
        False ->
            case maybe_frame of
                Just (NH2.Frame _  (NH2.GoAwayFrame _ ec _) ) -> do
                    case ec of
                        NH2.InternalError   -> return True
                        _                   -> return False

                _ ->
                    return False


frameIsGoAwayBecauseProtocolError :: DecoySession -> Bool -> Maybe NH2.Frame -> IO Bool
frameIsGoAwayBecauseProtocolError decoy_session prev maybe_frame = do
    case prev of
        True -> return True
        False ->
            case maybe_frame of
                Just (NH2.Frame _  (NH2.GoAwayFrame _ ec _) ) -> do
                    case ec of
                        NH2.ProtocolError   -> return True
                        _                   -> return False

                _ ->
                    return False


testUpdateWindowFrameAborts :: Test
testUpdateWindowFrameAborts = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context simpleWorker
    decoy_session <- createDecoySession attendant
    sendRawDataToSession decoy_session "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
    maybe_frame <- recvFrameFromSession decoy_session
    case maybe_frame of
        Nothing ->
            assertFailure "Waiting a frame, received none"
        Just (NH2.Frame _ (NH2.SettingsFrame _)) -> -- Ok
            return ()

        _ ->
            assertFailure "Waiting a settings frame, received something else"

    -- Send a settings frame now
    sendFrameToSession
        decoy_session
        ( NH2.EncodeInfo NH2.defaultFlags 0 Nothing,
          NH2.SettingsFrame [] )

    -- And now send a WindowUpdate frame
    sendFrameToSession
        decoy_session
        (NH2.EncodeInfo NH2.defaultFlags 51 Nothing,
         NH2.WindowUpdateFrame 10 )

    -- Now we read a few frames
    seen <- return False
    f0 <- recvFrameFromSession decoy_session
    seen1 <- frameIsGoAwayBecauseProtocolError decoy_session seen f0

    f1 <- recvFrameFromSession decoy_session
    seen2 <- frameIsGoAwayBecauseInternalError decoy_session seen1 f1

    -- f2 <- recvFrameFromSession decoy_session
    -- seen3 <- frameIsGoAwayBecauseProtocolError decoy_session seen2 f2

    -- f3 <- recvFrameFromSession decoy_session
    -- seen4 <- frameIsGoAwayBecauseProtocolError decoy_session seen3 f3

    if not seen2 then do
        assertFailure "Didn't see GoAwayFrame"
    else
        return ()

    -- Check session is alive
    got_error <- readMVar errors_mvar
    if got_error then do
        assertFailure "Exception raised unexpectedly"
    else
        return ()

testClosedInteraction0 :: Test
testClosedInteraction0 = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context simpleWorker
    decoy_session <- createDecoySession attendant
    let
        start_client =  decoy_session ^. startClientSessionCallback
    start_client
    got_error <- readMVar errors_mvar
    if got_error then do
        assertFailure "Exception raised unexpectedly"
    else
        return ()


testClosedInteraction1 :: Test
testClosedInteraction1 = TestCase $ do
    errors_mvar <- newMVar False
    sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
    let
        attendant = http2Attendant sessions_context simpleWorker
    decoy_session <- createDecoySession attendant
    let
        start_client =  decoy_session ^. startClientSessionCallback
    client_state <- start_client
    got_error <- readMVar errors_mvar
    if got_error then do
        assertFailure "Exception raised unexpectedly"
    else
        return ()

    (headers, input_data_stream) <- request client_state simpleRequestHeaders (return ())
    if length headers <= 0
      then
        assertFailure "NoHeadersBack"
      else
        return ()
    return ()


-- TODO: Apparently the client exception is not good enough, improve.
-- testClosedInteraction3 :: Test
-- testClosedInteraction3 = TestCase $ do
--     errors_mvar <- newMVar False
--     sessions_context <- makeSessionsContext (errorsSessionConfig errors_mvar)
--     let
--         attendant = http2Attendant sessions_context simpleWorker
--     decoy_session <- createDecoySession attendant
--     let
--         start_client =  decoy_session ^. startClientSessionCallback
--     client_state <- start_client
--     got_error <- readMVar errors_mvar

--     ee_mvar <- newMVar False
--     catch
--         (do
--             request client_state badRequestHeaders (return ())
--             return ()
--         )
--         ((\ _ -> modifyMVar_ ee_mvar ( \ _ -> return $ True)  ):: HTTP2SessionException -> IO ()  )
--     if got_error then do
--         return ()
--     else
--         assertFailure "IWasExpectingAnError"
--     ee <- takeMVar ee_mvar
--     if ee
--       then
--         return ()
--       else
--         assertFailure "IWasExpectingAnError--"
