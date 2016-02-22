{-# LANGUAGE TemplateHaskell  #-}
{-

Network performance throtler.

This module helps simulating bandwidth and latency in an output port.

-}
module SecondTransfer.IOCallbacks.Throtler (
                 newThrotler
    ) where


import           Control.Lens                           hiding ( (:<), (|>) )
--import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad                          (when)
import           Control.Monad.Trans.Reader
import           Control.Concurrent
--import           Control.Concurrent.Chan

import           Data.IORef
import qualified Data.ByteString.Lazy                         as LB


import           System.Clock                            ( getTime
                                                         , Clock(..)
                                                         , TimeSpec
                                                         , timeSpecAsNanoSecs
                                                         )

import           Data.Sequence
import qualified Data.Sequence                          as Sq


--import qualified Data.ByteString                        as B

import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.Exception


-- |Some number that says something about the precission of the throtler. It is the
-- length of the log. Let's say 20
lengthOfPacketLog :: Int
lengthOfPacketLog = 20


data ThrottlerState = ThrottlerState {
    _down_TS           :: IOCallbacks
  , _latency_TS        :: Double
  , _bandwidth_TS      :: Double

  , _waitingPackets_TS :: Chan (TimeSpec, LB.ByteString)

  , _sentLog_TS        :: IORef (Seq (TimeSpec, Int))
    }

makeLenses ''ThrottlerState

type Throttler = ReaderT  ThrottlerState IO


-- | Creates a new throtler. The bandwidth should be given in bytes per
--   second, and the latency in seconds (or most likely, fractions of
--   a second). Notice that this throtler will buffer  an unlimitted
--   number of bytes in RAM while operating, so it should not be used
--   in production settings.
newThrotler :: Double -> Double -> IOCallbacks -> IO IOCallbacks
newThrotler bandwidth latency sourceio =
  do
    waiting_chan <- newChan
    sent_log_ioref <- newIORef mempty
    let
        throttler_state = ThrottlerState {
            _down_TS    = sourceio
          , _latency_TS = latency
          , _bandwidth_TS = bandwidth
          , _waitingPackets_TS = waiting_chan
          , _sentLog_TS = sent_log_ioref
        }

    -- Start the throtler thread
    _ <- forkIOExc "throtlerThread"
        $ ignoreException blockedIndefinitelyOnMVar ()
        $ runReaderT deliveryThread throttler_state

    -- Create a new set of callbacks and return it
    return $ set pushAction_IOC (\ x -> runReaderT (throtlerPush x) throttler_state) sourceio



throtlerPush ::  LB.ByteString -> Throttler ()
throtlerPush contents =
  do
    -- When the packet arrived
    when_arrived <- liftIO $ getTime Monotonic
    waiting_chan <- view waitingPackets_TS
    liftIO $ writeChan waiting_chan (when_arrived, contents)


-- | works like t2 - t1
secondsDiff :: TimeSpec -> TimeSpec -> Double
secondsDiff t2 t1 =
   ( fromIntegral (timeSpecAsNanoSecs $ t2 - t1)  ) * 1.0e-9


-- | The thread in charge of delivering packets
deliveryThread :: Throttler ()
deliveryThread =
  do
    waiting_chan <- view waitingPackets_TS
    -- Ok, let's see when we can deliver the next packet
    next_to_deliver <- liftIO $ readChan waiting_chan

    -- There are two variables that I need to take into account: one
    -- limit set by bandwidth, and one limit set by latency
    now <- liftIO $ getTime Monotonic

    latency <- view latency_TS
    bandwidth <- view bandwidth_TS

    -- Let's calculate the moment in the future when we can deliver
    -- the current packet according to latency
    let
        (packet_set_time, packet_contents) = next_to_deliver
        latency_future =  latency - ( now `secondsDiff`  packet_set_time )

    sent_log_ioref <- view sentLog_TS
    sent_log <- liftIO $ readIORef sent_log_ioref

    -- And now let's calculate the moment in the future when we
    -- can deliver the packet according to bandwidth.
    -- For that, we need to take the timespan
    let
        size_to_deliver = fromIntegral . LB.length $ packet_contents

        bandwidth_future = case  viewl sent_log of
            EmptyL ->
                -- No packets has been sent, the admisible future
                -- is zero
                0

            -- The oldest sent packet can be found at the left of the
            -- sent log.
            ( (when_sent, _) :< _after_that ) ->
                -- We need to apply the formula
                let
                    -- t0 should be negative implying a negative offset
                    -- from now, so that the time tnp1 that
                    -- we get implies a likely possitive offset from now.
                    t0 =  when_sent `secondsDiff` now
                    -- Number of bytes that appear in the log as sent in the
                    -- time-interval that the log covers.
                    bytes_sent_before = foldl (\x p -> snd p + x) 0 sent_log

                    -- This is the formula for the time in the future when we can
                    -- send the next packet according to bandwidth. I got this formula
                    -- in a piece of paper, and it includes the time of the oldest
                    -- sent packet ...
                    tnp1 = t0 + (fromIntegral (bytes_sent_before +  size_to_deliver))/bandwidth
                in max 0.0 tnp1

        -- The time to wait is the max of the time indicated by the latency
        -- and by the bandwidth calculation.
        what_to_wait = max latency_future bandwidth_future

    -- Ok, I can decide now what to do. In some cases, I may not need
    -- to wait anything.
    when (what_to_wait > 0) $ do
         -- I will have to wait a few microseconds
        liftIO $ threadDelay (floor $ what_to_wait * 1000000)

    -- Let's store when I could deliver the current packet (which remember,
    -- we just got from a queue)
    new_now <- liftIO $ getTime Monotonic

    let
        new_sent_log = if Sq.length sent_log < lengthOfPacketLog
           then sent_log |> (new_now, size_to_deliver)
           else Sq.drop 1 sent_log |> (new_now, size_to_deliver)

    -- Update the sent log
    liftIO $ writeIORef sent_log_ioref new_sent_log

    -- Finally send the packet
    push_action <- view (down_TS . pushAction_IOC )
    liftIO $ push_action packet_contents

    -- And iteratively re-enter the place
    deliveryThread
