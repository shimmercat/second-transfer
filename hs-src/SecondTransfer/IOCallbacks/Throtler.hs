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
import qualified Control.Exception                      as E
import           Control.Monad                          (when)
import           Control.Monad.Trans.Reader
import           Control.Concurrent
import           Control.Concurrent.BoundedChan
import qualified Control.Concurrent.BoundedChan         as BC
--import qualified Control.Monad.Catch                    as CMC

import           Data.IORef
import qualified Data.ByteString.Lazy                   as LB


import           System.Clock                            ( getTime
                                                         , Clock(..)
                                                         , TimeSpec
                                                         , toNanoSecs
                                                         )

import           Data.Sequence
import qualified Data.Sequence                          as Sq


--import qualified Data.ByteString                        as B

import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.Exception

--import           Debug.Trace


data ThrottlerState = ThrottlerState {
    _down_TS           :: IOCallbacks
  , _latency_TS        :: Double
  , _bandwidth_TS      :: Double

  , _waitingPackets_TS :: BoundedChan (TimeSpec, LB.ByteString)

  , _sentLog_TS        :: IORef (Seq (TimeSpec, Int))
  , _logLength_TS      :: Int
  , _problemSending_TS :: MVar ()
    }

makeLenses ''ThrottlerState

type Throttler = ReaderT  ThrottlerState IO


-- Used in the various calculations below
assumedAveragePacketLength :: Double
assumedAveragePacketLength = 8192.0

-- | How many packets do we need to store to simulate network parameters?
lengthOfPacketBacklog :: Double -> Double -> Int
lengthOfPacketBacklog bandwidth latency  =
  let
    bytes_to_store = bandwidth * latency
  in
    max 1 (floor $ bytes_to_store / assumedAveragePacketLength)


-- | Let's make this go back half a second. That is, the length of the
--   record should be such that we can contain the number of packets of
--   length "assumedAveragePacketLength" that we would be able to send in
--   half a second, or one. This only uses the bandwidth, not the latency.
lengthOfSentPacketsRecord :: Double -> Double -> Int
lengthOfSentPacketsRecord bandwidth _latency =
  let
    bytes_of_half_a_second = 0.5 * bandwidth
  in
    max 1 (ceiling $ bytes_of_half_a_second / assumedAveragePacketLength)


-- | Creates a new throtler. The bandwidth should be given in bytes per
--   second, and the latency in seconds (or most likely, fractions of
--   a second). Notice that this throtler will buffer  an unlimitted
--   number of bytes in RAM while operating, so it should not be used
--   in production settings.
newThrotler :: Double -> Double -> IOCallbacks -> IO IOCallbacks
newThrotler bandwidth latency sourceio =
  do
    waiting_chan <- newBoundedChan $ lengthOfPacketBacklog bandwidth latency
    sent_log_ioref <- newIORef mempty
    problem_sending <- newEmptyMVar
    let
        throttler_state = ThrottlerState {
            _down_TS    = sourceio
          , _latency_TS = latency
          , _bandwidth_TS = bandwidth
          , _waitingPackets_TS = waiting_chan
          , _sentLog_TS = sent_log_ioref
          , _logLength_TS = lengthOfSentPacketsRecord bandwidth latency
          , _problemSending_TS = problem_sending
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
    maybe_problem_mvar <- view problemSending_TS
    maybe_problem <- liftIO . tryReadMVar $ maybe_problem_mvar
    case maybe_problem of
        Nothing ->
            liftIO $ BC.writeChan waiting_chan (when_arrived, contents)

        Just _ ->
            liftIO . E.throw $ NoMoreDataException


-- | works like t2 - t1
secondsDiff :: TimeSpec -> TimeSpec -> Double
secondsDiff t2 t1 =
   ( fromIntegral (toNanoSecs $ t2 - t1)  ) * 1.0e-9


-- | The thread in charge of delivering packets
deliveryThread :: Throttler ()
deliveryThread =
  do
    waiting_chan <- view waitingPackets_TS
    -- Ok, let's see when we can deliver the next packet
    next_to_deliver <- liftIO $ BC.readChan waiting_chan

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

        --  trace ("size to deliver " ++ show size_to_deliver) $

        bandwidth_future = case  viewl sent_log of
            EmptyL ->
                -- No packets has been sent, the admisible future
                -- is zero
                0

            -- The oldest sent packet can be found at the left of the
            -- sent log. BUG: This fails when the connection has been inactive
            -- for a long time, since the history present for all the packets
            -- registered in the back-log consistently says that there is space
            ( (when_sent, _) :< _after_that ) ->
                -- We need to apply the formula
                let
                    -- t0 should be negative implying a negative offset
                    -- from now, so that the time tnp1 that
                    -- we get implies a likely possitive offset from now.
                    t0 =   when_sent `secondsDiff` now
                    -- Number of bytes that appear in the log as sent in the
                    -- time-interval that the log covers.
                    bytes_sent_before = foldl (\x p -> snd p + x) 0 sent_log

                    -- This is the formula for the time in the future when we can
                    -- send the next packet according to bandwidth. I got this formula
                    -- in a piece of paper, and it includes the time of the oldest
                    -- sent packet ...
                    tnp1 = t0 + (fromIntegral (bytes_sent_before +  size_to_deliver))/bandwidth
                in  max 0.0 tnp1

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

    length_of_packet_record <- view logLength_TS

    problem_mvar <- view problemSending_TS

    let
        new_sent_log = if Sq.length sent_log < length_of_packet_record
           then sent_log |> (new_now, size_to_deliver)
           else Sq.drop 1 sent_log |> (new_now, size_to_deliver)

        set_problem = do
           tryPutMVar problem_mvar ()

    -- Update the sent log
    liftIO $ writeIORef sent_log_ioref new_sent_log

    -- Finally send the packet
    push_action <- view (down_TS . pushAction_IOC )

    should_continue <- liftIO $ E.catches
       (do
             push_action packet_contents
             return True
       )
       [
         E.Handler ((\ _ex -> set_problem >> return False  ) :: IOProblem -> IO Bool),
         E.Handler ((\ _ex -> set_problem >> return False  ) :: E.BlockedIndefinitelyOnMVar -> IO Bool )
       ]


    -- And iteratively re-enter the place
    when should_continue $
        deliveryThread
