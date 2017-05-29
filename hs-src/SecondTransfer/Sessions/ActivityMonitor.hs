{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings, BangPatterns #-}
{--

Helps measuring activity

--}
module SecondTransfer.Sessions.ActivityMonitor (
                     ActivityMonitor                    (..)
                   , newActivityMonitor
                   , waitForIdle
                   , getLastActivity
                   , reportEvent
                   ) where



import               Data.Int                             (Int64)
import               System.Clock
import               Control.Lens
import               Control.Concurrent
import               Control.Concurrent.MVar

-- import               Debug.Trace


-- | When in the past was the last activity reported.
--   item_0_ms is the most recent.
data ActivityLog = ActivityLog {
     _item0_ms ::  !Int64
    ,_item1_ms ::  !Int64
    ,_item2_ms ::  !Int64
}


-- | Moves the values back and insert a new one.
cycleActivityLog :: ActivityLog -> Int64 -> ActivityLog
cycleActivityLog
    act_log@(ActivityLog {
        _item0_ms = i0,
        _item1_ms = i1,
        _item2_ms = _i2
    }) new_time =
    if new_time > i0 then
        ActivityLog {
            _item0_ms = new_time,
            _item1_ms = i0,
            _item2_ms = i1
        }
     else
        act_log


-- | Checks if all the three time-intervals that can be constructed
--   from present_time and the values in the activity monitor are lower than
--   the threshold. If so, we say this tick is "busy"
--
--   (The threshold is given in nanoseconds)
isBusyTick :: ActivityLog -> Int64 -> Int64 -> Bool
isBusyTick
    (ActivityLog {
        _item0_ms = i0,
        _item1_ms = _i1,
        _item2_ms = _i2
    }) new_time
       threshold
  =
  let
    interval0 = new_time - i0
    -- interval1 = i0 - i1
    -- interval2 = i1 - i2
  in
    interval0 < (2 * threshold)


-- | The easy-to-use API from the session
data ActivityMonitor = ActivityMonitor {
    _activityLog_AM  ::   MVar ActivityLog
    }


newActivityMonitor :: IO ActivityMonitor
newActivityMonitor =
  do
    current_time <- getClock
    al <- newMVar $ ActivityLog current_time current_time current_time
    return $ ActivityMonitor { _activityLog_AM = al }


-- | Just logs the fact that there was some activity
reportEvent :: ActivityMonitor -> IO ()
reportEvent am =
  do
    current_time <- getClock
    modifyMVar_ (_activityLog_AM am) $
      \ act_log -> return (cycleActivityLog act_log current_time)


-- | Reason the wait ended:
data WaitEndReason =
     Timeout_WER  -- ^ We got bored of waiting, this is going to hurt.
    |Idle_WER     -- ^ There is nothing-much happening.
    deriving (Show, Eq, Ord)


-- | Waits a predetermined amount of time for not-activity
waitForIdle :: ActivityMonitor -> Int64 -> Int64 -> IO WaitEndReason
waitForIdle
   activity_monitor
   must_idle_nanoseconds  -- ^ How many nanoseconds it must idle
   wait_timeout           -- ^ Don't wait more than this.
  =
   do
     current_time <- getClock
     al <- readMVar (_activityLog_AM activity_monitor)
     let
         is_busy_tick =
            isBusyTick
                al
                current_time
                must_idle_nanoseconds
     if is_busy_tick
       then
         if wait_timeout > must_idle_nanoseconds
           then do
              -- Wait some more
              threadDelay (fromIntegral $ 2 * must_idle_nanoseconds `div` 1000)
              waitForIdle activity_monitor must_idle_nanoseconds (wait_timeout - must_idle_nanoseconds)
           else
              -- Waited too much
              return Timeout_WER

       else
         -- Is not busy, say we are fine
         return Idle_WER


getLastActivity :: ActivityMonitor -> IO TimeSpec
getLastActivity activity_monitor =
  do
    al <- readMVar (_activityLog_AM activity_monitor)
    let
        t0 = _item0_ms al
    return . fromNanoSecs . fromIntegral $ t0


-- | Get the wall time as some sort of integer
getClock :: IO Int64
getClock =
  do
      ts <- getTime Monotonic
      return ( (sec ts)*1000000000 + nsec ts )
