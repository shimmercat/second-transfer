{-# LANGUAGE OverloadedStrings #-}
module Tests.HTTP2Session where 

import Test.HUnit    


import Control.Concurrent.MVar
import Control.Lens
import Control.Concurrent (threadDelay)

import SecondTransfer.Test.DecoySession
import SecondTransfer.Http2
import SecondTransfer


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
    threadDelay 2000000
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
    threadDelay 2000000
    got_error <- readMVar errors_mvar
    if not got_error then do 
        assertFailure "Exception didn't raise"
    else
        return ()
