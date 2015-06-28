{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module SecondTransfer.MainLoop.Logging (
    -- | Simple, no fuss enable logging
    enableConsoleLogging
    ,logWithExclusivity
    ,logit
    ) where

import           System.IO                 (stderr,openFile)
import qualified System.IO                 as SIO
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as Bch

-- Logging utilities
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter, LogHandler)
import           System.Log.Handler.Simple
-- import           System.Log.Handler.Syslog (Facility (..), Option (..), openlog)
import           System.Log.Logger
import           System.IO.Unsafe          (unsafePerformIO)
import           System.Clock              as Cl

import           Control.Concurrent.MVar
import           Control.Concurrent.Chan
import           Control.Concurrent


-- | Activates logging to terminal
enableConsoleLogging :: IO ()
enableConsoleLogging = configureLoggingToConsole


-- | Protect logging with a mutex... that is to say,
--   this is a horrible hack and you should try to log
--   as little as possible or nothing at all. This just
--   works for instrumentation locks...
globallyLogWell :: MVar ()
{-# NOINLINE globallyLogWell #-}
globallyLogWell = unsafePerformIO (newMVar () )

-- | Used internally to avoid garbled logs
logWithExclusivity :: IO () -> IO ()
logWithExclusivity a = withMVar globallyLogWell (const a )


configureLoggingToConsole :: IO ()
configureLoggingToConsole = do
    s <- streamHandler stderr DEBUG  >>=
        \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    setLoggerLevels s


-- configureLoggingToSyslog :: IO ()
-- configureLoggingToSyslog = do
--     s <- openlog "RehMimic" [PID] DAEMON INFO >>=
--         \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
--     setLoggerLevels s


setLoggerLevels :: (LogHandler s) => s -> IO ()
setLoggerLevels s = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger "Session" (
        setHandlers [s] .
        setLevel INFO
        )
    updateGlobalLogger "OpenSSL" (
        setHandlers [s] .
        setLevel DEBUG
        )
    updateGlobalLogger "HTTP1" (
        setHandlers [s] .
        setLevel DEBUG
        )
    updateGlobalLogger "HTTP2" (
        setHandlers [s] .
        setLevel DEBUG
        )



data Logit = Logit Cl.TimeSpec B.ByteString


loggerChan :: Chan Logit
{-# NOINLINE loggerChan #-}
loggerChan = unsafePerformIO $ do
    chan <- newChan
    log_file <- openFile "LOGIT" SIO.WriteMode
    SIO.hSetBuffering log_file SIO.LineBuffering
    start_of_time <- Cl.getTime Cl.Monotonic
    forkIO $ readLoggerChan chan log_file start_of_time
    return chan

readLoggerChan ::  Chan Logit -> SIO.Handle -> Cl.TimeSpec -> IO ()
readLoggerChan chan_logit file_handle origin_time = do
    Logit timespec bs <- readChan chan_logit
    let
        Cl.TimeSpec sec' nsec' = timespec - origin_time
    SIO.hPutStr file_handle (show sec')
    SIO.hPutStr file_handle "|"
    SIO.hPutStr file_handle (show nsec')
    SIO.hPutStr file_handle "|"
    Bch.hPutStrLn file_handle bs
    SIO.hFlush file_handle
    readLoggerChan chan_logit file_handle origin_time

-- Simple logging function. It logs everything to a file named
-- "logit" in the current directory, adding a time-stamp
logit :: B.ByteString -> IO ()
logit !msg = do
    time <- Cl.getTime Cl.Monotonic
    let
        lg = Logit time msg
    writeChan loggerChan lg
