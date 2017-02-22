{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module SecondTransfer.Http2.PriorityChannels (
                  putInPriorityChannel
                , getDataUpTo
                , PriorityChannelsState
                , newPriorityChannelState
                , putInPriorityChannelWithCallback
    ) where

import            Control.Lens
import            Control.Concurrent
import            Control.Monad.IO.Class                       (liftIO)
import            Control.Monad.Trans.Reader
import qualified  Control.Concurrent.BoundedChan                as A

import qualified Data.ByteString.Lazy                           as LB
import qualified Data.ByteString.Builder                        as Bu

import qualified Data.Map.Strict                                as DM

import qualified SecondTransfer.Http2.Constants                 as CONSTANT

import           Debug.Trace

-- | A channel key (SystemPriority, OrdinaryPriority, StreamId)
newtype ChannelKey = ChannelKey (Int, Int, Int)
    deriving (Eq, Ord, Show)

-- | This is the information coming in each channel
data ChannelToken = ChannelToken {
    _payload_ChT         :: ! LB.ByteString
  , _streamId_ChT        :: ! Int
  , _streamOrdinal_ChT   :: ! Int
  -- | Delivery callback
  , _notifyDelivery_ChT  :: ! (Maybe (IO ()))
    }

makeLenses ''ChannelToken


-- | The larger this number be the lower the chance of having packets
--   delivered in an order which is against the set priorities.
--   And the more memory we will use buffering contents.
boundedChanSize :: Int
boundedChanSize = 5


-- | Ok, here is where the data will come...
type Gateways = DM.Map ChannelKey (A.BoundedChan ChannelToken)


data PriorityChannelsState = PriorityChannelsState {
    _gateways_PCS                 :: MVar Gateways

    -- Notify the channel that there is more data available.
  , _dataReady_PCS                :: MVar ()
    }

makeLenses ''PriorityChannelsState


newPriorityChannelState :: IO PriorityChannelsState
newPriorityChannelState =
  do
    gateways_mvar <- newMVar DM.empty
    data_ready <- newEmptyMVar

    return PriorityChannelsState {
        _gateways_PCS = gateways_mvar
      , _dataReady_PCS = data_ready
        }


-- | Execute things here
type PriorityChannelM = ReaderT PriorityChannelsState IO


-- | Nice way to get back to the IO Monad
runPriorityChannel ::  PriorityChannelsState ->  PriorityChannelM a -> IO a
runPriorityChannel state comp = runReaderT comp state

putInPriorityChannel :: PriorityChannelsState -> Int -> Int -> Int -> Int -> LB.ByteString -> IO ()
putInPriorityChannel state system_priority priority stream_id packet_ordinal datum = runPriorityChannel
    state (putInPriorityChannel_AtM system_priority priority stream_id packet_ordinal datum Nothing)


putInPriorityChannelWithCallback :: PriorityChannelsState -> IO () -> Int -> Int -> Int -> Int -> LB.ByteString -> IO ()
putInPriorityChannelWithCallback state callback system_priority priority stream_id packet_ordinal datum = runPriorityChannel
    state (putInPriorityChannel_AtM system_priority priority stream_id packet_ordinal datum (Just callback))



-- | If necessary, builds a channel at the given priority level, and puts a token
--   there.
--
-- This function may block (which is desired)
--
--   Notice that for this to work, the calm of the packets in a stream should be non-
--  decreasing.
putInPriorityChannel_AtM ::
    Int ->
    Int ->
    Int ->
    Int ->
    LB.ByteString ->
    Maybe (IO () ) ->
    PriorityChannelM ()
putInPriorityChannel_AtM
    system_priority
    priority
    stream_id
    packet_ordinal
    datum
    maybe_callback
  =
  do

    -- Run this slower
    liftIO $ yield
    gateways_mvar <- view gateways_PCS

    let
        channel_key = ChannelKey (system_priority, priority, stream_id)
        token = ChannelToken datum stream_id packet_ordinal maybe_callback

    channel <- liftIO . modifyMVar gateways_mvar $ \ gateways -> do
        let
            f1 = DM.lookup channel_key gateways

        -- Good point to be sure that performance is properly degraded
        -- of the output vs input side
        yield

        (gw2, channel_mvar) <- case f1 of
            Just chm -> return (gateways, chm)

            Nothing -> do
               new_channel <-  A.newBoundedChan boundedChanSize
               let
                   gw2' = DM.insert channel_key new_channel gateways
               return (gw2', new_channel )

        -- again
        yield

        return (gw2 , channel_mvar )

    -- Signal that there is data
    ready_mvar <- view dataReady_PCS

    -- Make it slower
    liftIO yield

    -- As soon as we succeed on actually putting the data into the channel, notify
    -- of it.
    liftIO $ do
        A.writeChan channel token
        yield
        _ <-  tryPutMVar ready_mvar ()
        yield

    return ()


-- | Gets the data with higher priority..., return the data and a bool indicating
--   if the data is a data packet (True for data packet)
getHigherPriorityData_AtM :: Bool -> PriorityChannelM (Bool, LB.ByteString)
getHigherPriorityData_AtM can_take_data =
  do
    gateways_mvar <- view gateways_PCS
    data_ready <- view dataReady_PCS
    maybe_token <- liftIO . withMVar gateways_mvar $ \ gateways ->
        gowy can_take_data gateways

    case maybe_token of

        Nothing -> do
            -- Wait for more data, no matter what.
            _ <- liftIO $ takeMVar data_ready
            getHigherPriorityData_AtM can_take_data

        Just (_channel_key@(ChannelKey (sys_prio, _ord_prio, _stream_id)), token) -> do
            case token ^. notifyDelivery_ChT of
                Nothing -> return ()
                Just clbk -> liftIO $ clbk
            return $ ( (sys_prio == 0) , token ^. payload_ChT)


-- | Same signature as before.
maybeGetHigherPriorityData_AtM :: Bool ->  PriorityChannelM (Maybe  (Bool,  LB.ByteString))
maybeGetHigherPriorityData_AtM can_take_data =
  do
    gateways_mvar <- view gateways_PCS
    maybe_token <- liftIO . withMVar gateways_mvar $ \ gateways ->
        gowy can_take_data gateways

    case maybe_token of
        Nothing -> do
            return Nothing

        Just (_channel_key@(ChannelKey (sys_prio, _ord_prio, _stream_id)), token) -> do
            case token ^. notifyDelivery_ChT of
                Nothing -> return ()
                Just clbk -> liftIO $ clbk
            return . Just $ ( (sys_prio == 0) , token ^. payload_ChT)


-- | Gets data from the priority tree. There are two boundaries:
--
--   `return_at_trigger` is the trigger to stop, and it is related
--    to the available buffer size.
--
--   `max_data` is the max amount of data with system priority zero,
--   corresponding to DATA HTTP/2 frames. This is given here to enforce
--   flow control.
--
--   We return the data plus a value to adjust the data credits downward
--   for flow control on the connection.
getDataUpTo ::
    PriorityChannelsState ->
    Int ->
    Int -> IO (Bu.Builder, Int)
getDataUpTo state return_at_trigger max_data =
    runPriorityChannel
       state
       (getDataUpTo_AtM True return_at_trigger max_data)


-- | Gets  the available data or up to the number of bytes that
--   falls in a frame boundary immediately after "return_at_trigger"
getDataUpTo_AtM :: Bool ->  Int -> Int  -> PriorityChannelM (Bu.Builder, Int)
getDataUpTo_AtM is_first return_at_trigger max_data
  | is_first =
      do
        let
            can_take_data = max_data >= CONSTANT.sessionFlowControlHighTide
        (is_data, datum) <- getHigherPriorityData_AtM can_take_data
        let
            datum_length = fromIntegral $  LB.length datum
            datum_as_bu = Bu.lazyByteString datum
            data_credits_down = if is_data
              then datum_length - 9
              else 0
        if datum_length > return_at_trigger
          then do
            return (datum_as_bu, data_credits_down)
          else do
            let
                new_trigger = return_at_trigger - datum_length
                new_max_data = max_data - data_credits_down

            (more_data, more_data_credits_down) <-
               getDataUpTo_AtM False new_trigger new_max_data
            return $
               (
                  (datum_as_bu `mappend` more_data),
                  (data_credits_down + more_data_credits_down)
               )
  | otherwise =
      do
        let
            can_take_data = max_data >= CONSTANT.sessionFlowControlHighTide
        maybe_datum <- maybeGetHigherPriorityData_AtM can_take_data
        case maybe_datum of
            Just (is_data, datum) -> do
                let
                    datum_length = fromIntegral $  LB.length datum
                    datum_as_bu = Bu.lazyByteString datum
                    data_credits_down = if is_data
                      then datum_length - 9
                      else 0
                if datum_length > return_at_trigger
                  then
                    return (datum_as_bu, data_credits_down)
                  else do
                    let
                        new_trigger = return_at_trigger - datum_length
                        new_max_data = max_data - data_credits_down
                    (more_data, more_credits_down) <-
                        getDataUpTo_AtM False new_trigger new_max_data
                    return $
                        (
                            datum_as_bu `mappend` more_data,
                            (
                              more_credits_down + data_credits_down
                            )
                        )

            Nothing -> return (mempty, 0)


gowy :: Bool ->  Gateways -> IO (Maybe (ChannelKey, ChannelToken))
gowy can_take_data  gw =
    case DM.minViewWithKey gw of
        Nothing ->  return Nothing

        Just ((ck@(ChannelKey (system_prio, _normal_prio, _stream_id)), a_channel), gw1)
          | system_prio < 0 || can_take_data -> do
            maybe_token <- A.tryReadChan a_channel
            case maybe_token of
                Nothing ->
                    -- Try to take the datum from the next
                    -- one
                    gowy can_take_data gw1

                Just token -> return $ Just (ck, token)

          | otherwise  -> return Nothing
