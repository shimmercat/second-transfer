{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module SecondTransfer.MainLoop.DebugMonitor (
    -- This is a simple monitoring utility..
#ifdef SECONDTRANSFER_MONITORING
                 incCounter
#endif
     ) where


#ifdef SECONDTRANSFER_MONITORING
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Concurrent.Chan

import           Control.Monad             (unless)

import           Data.IORef
import           Data.String
import           System.IO                 (stderr,openFile)
import qualified System.IO                 as SIO
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as Bch

import           System.IO.Unsafe          (unsafePerformIO)
import           System.Clock              as Cl
import           System.Posix.Files
import qualified Database.Redis            as R


{-# NOINLINE outputHandle  #-}
outputHandle :: IORef R.Connection
outputHandle = unsafePerformIO $ do
    let
        conn_info = R.defaultConnectInfo {
            R.connectDatabase = 3 -- Let's take this cone
            }
    conn <- R.connect conn_info
    R.runRedis conn $ R.flushdb
    newIORef conn

incCounter :: B.ByteString -> IO ()
incCounter keyname = do
    conn <- readIORef outputHandle
    R.runRedis conn (R.incr keyname)
    return ()

#endif
