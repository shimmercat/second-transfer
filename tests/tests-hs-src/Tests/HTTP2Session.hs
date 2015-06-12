{-# LANGUAGE OverloadedStrings #-}
module Tests.HTTP2Session where 


import           Test.HUnit
import qualified Network.HTTP2                    as NH2
import           Control.Concurrent               (threadDelay)
import qualified Control.Concurrent               as C(yield)
import           Control.Concurrent.MVar
import           Control.Lens

import           SecondTransfer.Http2             (http2Attendant)
import           SecondTransfer.Sessions
import           SecondTransfer.Test.DecoySession
import           SecondTransfer.Types




import Data.Conduit(yield)


saysHello :: DataAndConclusion
saysHello = do 
    yield "Hello world!\ns"
    -- No footers
    return []


simpleWorker :: CoherentWorker
simpleWorker request = return (
    [
        (":status", "200")
    ],
    [], -- No pushed streams
    saysHello
    )


setError :: MVar Bool -> ErrorCallback
setError mvar = \ error_info -> do 
    -- putStrLn $ show error_info
    modifyMVar_ mvar (\ _ -> return True )


errorsSessionConfig :: MVar Bool -> SessionsConfig
errorsSessionConfig mvar = set (sessionsCallbacks . reportErrorCallback) 
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
    if got_error then do 
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
        ( (NH2.EncodeInfo NH2.defaultFlags (NH2.toStreamIdentifier 0) Nothing),
          (NH2.SettingsFrame []) )

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

    let d2 = ( (NH2.EncodeInfo NH2.defaultFlags (NH2.toStreamIdentifier 0) Nothing),
          (NH2.PingFrame "01234567") )

    -- Receive a frame from the session, otherwise it won't even move

    -- Send a ping frame now, so that we get an error
    sendFrameToSession 
        decoy_session 
        d2

    -- sendFrameToSession 
    --     decoy_session 
    --     d2

    -- We need to give some time to the framework to react to problems
    threadDelay 20000

    -- Check session is alive
    got_error <- readMVar errors_mvar
    if not got_error then do 
        assertFailure "Exception didn't raise properly"
    else
        return ()        