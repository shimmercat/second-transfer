{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module SecondTransfer.Http2.PriorityChannels (
                  putInPriorityChannel
                , getDataUpTo
                , PriorityChannelsState
                , newPriorityChannelState
    ) where

import            Control.Lens
import            Control.Concurrent
import            Control.Monad.IO.Class                 (liftIO)
import            Control.Monad.Trans.Reader

import qualified Data.ByteString.Lazy                           as LB
import qualified Data.ByteString.Builder                        as Bu
-- import           Data.IORef

import qualified Data.Map.Strict                                as DM

import           Debug.Trace

-- | A channel key (SystemPriority, OrdinaryPriority)
newtype ChannelKey = ChannelKey (Int, Int)
    deriving (Eq, Ord, Show)

-- | This is the information coming in each channel
data ChannelToken = ChannelToken {
    _payload_ChT         :: ! LB.ByteString
  , _streamId_ChT        :: ! Int
  , _streamOrdinal_ChT   :: ! Int
    }

makeLenses ''ChannelToken



-- | Ok, here is where the data will come...
type Gateways = DM.Map ChannelKey (MVar ChannelToken)


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
    state (putInPriorityChannel_AtM system_priority priority stream_id packet_ordinal datum)


-- | If necessary, builds a channel at the given priority level, and puts a token
--   there.
--
--   Notice that for this to work, the calm of the packets in a stream should be non-
--  decreasing.
putInPriorityChannel_AtM :: Int -> Int -> Int -> Int -> LB.ByteString -> PriorityChannelM ()
putInPriorityChannel_AtM system_priority priority stream_id packet_ordinal datum =
  do
    gateways_mvar <- view gateways_PCS

    let
        channel_key = ChannelKey (system_priority, priority)
        token = ChannelToken datum stream_id packet_ordinal

    channel <- liftIO . modifyMVar gateways_mvar $ \ gateways -> do
        let
            f1 = DM.lookup channel_key gateways

        (gw2, channel_mvar) <- case f1 of
            Just chm -> return (gateways, chm)

            Nothing -> do
               new_channel <-  newEmptyMVar
               let
                   gw2' = DM.insert channel_key new_channel gateways
               return (gw2', new_channel )

        return (gw2 , channel_mvar )

    -- Signal that there is data
    ready_mvar <- view dataReady_PCS

    -- As soon as we succeed on actually putting the data into the channel, notify
    -- of it.
    liftIO $ putMVar channel token
    _ <- liftIO $ tryPutMVar ready_mvar ()

    return ()


-- | Gets the data with higher priority...
getHigherPriorityData_AtM :: PriorityChannelM LB.ByteString
getHigherPriorityData_AtM =
  do
    gateways_mvar <- view gateways_PCS
    data_ready <- view dataReady_PCS
    maybe_token <- liftIO . withMVar gateways_mvar $ \ gateways ->
        gowy gateways

    case maybe_token of

        Nothing -> do
            -- Wait for more data
            _ <- liftIO $ takeMVar data_ready
            getHigherPriorityData_AtM

        Just token ->
            return $ token ^. payload_ChT


maybeGetHigherPriorityData_AtM :: PriorityChannelM (Maybe LB.ByteString)
maybeGetHigherPriorityData_AtM =
  do
    gateways_mvar <- view gateways_PCS
    maybe_token <- liftIO . withMVar gateways_mvar $ \ gateways ->
        gowy gateways

    case maybe_token of

        Nothing -> do
            return Nothing


        Just token ->
            return . Just $ token ^. payload_ChT


getDataUpTo :: PriorityChannelsState ->  Int -> IO Bu.Builder
getDataUpTo state return_at_trigger = runPriorityChannel state (getDataUpTo_AtM True return_at_trigger)


-- | Gets  the available data or up to the number of bytes that
--   falls in a frame boundary immediately after "return_at_trigger"
getDataUpTo_AtM :: Bool ->  Int -> PriorityChannelM Bu.Builder
getDataUpTo_AtM is_first return_at_trigger
  | is_first =
      do
        datum <- getHigherPriorityData_AtM
        let
            datum_length = fromIntegral $  LB.length datum
            datum_as_bu = Bu.lazyByteString datum
        if datum_length > return_at_trigger
          then do
            liftIO $ putStrLn "cut-----------------"
            return datum_as_bu
          else do
            let
                new_trigger = return_at_trigger - datum_length
            more_data <- getDataUpTo_AtM False new_trigger
            return $ datum_as_bu `mappend` more_data
  | otherwise =
      do
        maybe_datum <- maybeGetHigherPriorityData_AtM

        case maybe_datum of
            Just datum -> do
                let
                    datum_length = fromIntegral $  LB.length datum
                    datum_as_bu = Bu.lazyByteString datum
                if datum_length > return_at_trigger
                  then
                    return datum_as_bu
                  else do
                    let
                        new_trigger = return_at_trigger - datum_length
                    more_data <- getDataUpTo_AtM False new_trigger
                    return $ datum_as_bu `mappend` more_data

            Nothing -> return mempty

gowy :: Gateways -> IO (Maybe ChannelToken)
gowy  gw =
    case DM.minView gw of
        Nothing ->  return Nothing

        Just (a_channel, gw1) -> do
            maybe_token <- tryTakeMVar a_channel
            case maybe_token of
                Nothing ->
                    -- Try to take the datum from the next
                    -- one
                    gowy gw1

                Just token -> return $ {- trace  ("token-fetched, length: " ++ (show . LB.length $ token ^. payload_ChT) ) $ -} Just token
