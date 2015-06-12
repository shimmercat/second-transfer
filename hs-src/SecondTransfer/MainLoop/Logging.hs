{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.MainLoop.Logging (
    -- | Simple, no fuss enable logging
    enableConsoleLogging
    ,logWithExclusivity
    ) where

import           System.IO                 (stderr)


-- Logging utilities
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter, LogHandler)
import           System.Log.Handler.Simple
-- import           System.Log.Handler.Syslog (Facility (..), Option (..), openlog)
import           System.Log.Logger
import           System.IO.Unsafe          (unsafePerformIO)

import           Control.Concurrent.MVar 


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
logWithExclusivity a = withMVar globallyLogWell (\_ -> a )


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

