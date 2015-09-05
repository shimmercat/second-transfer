{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module SecondTransfer.Test.DecoySession where



import           Control.Concurrent
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Lens            (view, (^.), over)
import qualified Control.Lens            as L
import           Control.Monad.STM       (atomically)

import qualified Network.HTTP2           as NH2
import qualified Network.HPACK           as HP

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as LB
import qualified Data.ByteString.Builder as Bu
import qualified Data.Sequence           as Sq
import           Data.Sequence           ( (<|), (|>), ViewL(..), Seq )

import           SecondTransfer.Http2
import           SecondTransfer.Types
import           SecondTransfer.MainLoop.Internal

-- "Internal imports"
import          SecondTransfer.MainLoop.CoherentWorker (defaultEffects)


data DataContinuityEngine = DataContinuityEngine {
    _incoming_DCE :: Seq B.ByteString
    }


L.makeLenses ''DataContinuityEngine


emptyDCE :: DataContinuityEngine
emptyDCE =  DataContinuityEngine  Sq.empty


dceAddData :: DataContinuityEngine -> B.ByteString -> DataContinuityEngine
dceAddData dce moredata | B.length moredata > 0 = over incoming_DCE ( |>  moredata ) dce
dceAddData dce moredata | otherwise             = dce


tryGetThisManyData :: DataContinuityEngine -> Int -> Maybe (DataContinuityEngine, B.ByteString)
tryGetThisManyData dce n = let
    go_result = go n "" (dce ^. incoming_DCE)
    go :: Int -> Bu.Builder -> Seq B.ByteString -> Maybe (Seq B.ByteString, Bu.Builder)
    go n bld seq  | Sq.EmptyL <- Sq.viewl seq, n > 0   = Nothing
                  | Sq.EmptyL <- Sq.viewl seq, n == 0  = Just (seq, bld)
                  | lead :< rest <- Sq.viewl seq, n > B.length lead    = go (n - B.length lead) (bld `mappend` Bu.byteString lead) rest
                  | lead :< rest <- Sq.viewl seq, n == B.length lead   = Just ( rest, bld `mappend` Bu.byteString lead)
                  | lead :< rest <- Sq.viewl seq, n < B.length lead    = let
                      (tk, lv) = B.splitAt n lead
                      in Just ( lv <| rest, bld `mappend`  Bu.byteString tk  )
  in case go_result of
    Just (new_dce, bld ) -> Just
        (DataContinuityEngine new_dce,  LB.toStrict . Bu.toLazyByteString $ bld )

    Nothing -> Nothing


dceIsEmpty :: DataContinuityEngine -> Bool
dceIsEmpty dce = Sq.null $ dce ^. incoming_DCE


dceGetAllData :: DataContinuityEngine -> (DataContinuityEngine, B.ByteString)
dceGetAllData dce = (emptyDCE, all_together)
  where
    all_together = LB.toStrict . Bu.toLazyByteString .  L.foldMapOf (incoming_DCE . L.folded) Bu.byteString $ dce


data DecoySession = DecoySession {
    _inputDataChannel   :: TChan B.ByteString
    ,_outputDataChannel :: TChan LB.ByteString
    ,_sessionThread     :: ThreadId
    ,_remainingOutputBit:: TMVar B.ByteString
    ,_nh2Settings       :: NH2.Settings
    ,_sessionThrowed    :: TMVar Bool
    ,_waiting           :: TMVar ThreadId
    ,_encodeHeadersHere :: TMVar HP.DynamicTable
    ,_decodeHeadersHere :: TMVar HP.DynamicTable

    ,_incomingData      :: TMVar DataContinuityEngine
    }


L.makeLenses ''DecoySession


-- Supposed to be an HTTP/2 attendant
createDecoySession :: Attendant -> IO DecoySession
createDecoySession attendant = do
    input_data_channel  <- newTChanIO
    output_data_channel <- newTChanIO
    incoming_data_tmvar  <- newTMVarIO emptyDCE
    session_throwed_tmvar <- newTMVarIO False
    let
        push_action  :: PushAction
        push_action lazy_bs  =  atomically $
            writeTChan output_data_channel lazy_bs

        pull_action  :: PullAction
        pull_action byte_count = atomically $ do
            incoming_data <- takeTMVar incoming_data_tmvar
            let
                go dce = let
                    maybe_stuff = tryGetThisManyData dce byte_count
                    in case maybe_stuff of
                        Nothing -> do
                            new_data <- readTChan input_data_channel
                            let
                                new_dce = dceAddData dce new_data
                            go new_dce

                        Just (new_bce, bs) -> do
                            putTMVar incoming_data_tmvar new_bce
                            return bs
            go incoming_data

        best_effort_pull_action :: BestEffortPullAction
        best_effort_pull_action can_block = atomically $ do
            incoming_data <- takeTMVar incoming_data_tmvar
            if dceIsEmpty incoming_data
              then
                if can_block
                  then do
                    -- Wait for the next item
                    new_data <- readTChan input_data_channel
                    -- Leave the empty thing alone
                    putTMVar incoming_data_tmvar incoming_data
                    return new_data
                  else do
                    putTMVar incoming_data_tmvar incoming_data
                    return ""
              else do
                  let
                      (new_dce, all_together) = dceGetAllData incoming_data
                  putTMVar incoming_data_tmvar new_dce
                  return all_together

        close_action :: CloseAction
        close_action =  return ()
        attendant_callbacks = AttendantCallbacks {
            _pushAction_AtC = push_action,
            _pullAction_AtC = pull_action,
            _closeAction_AtC = close_action,
            _bestEffortPullAction_AtC = best_effort_pull_action
            }

    dtable_for_encoding <- HP.newDynamicTableForEncoding 4096
    dtable_for_encoding_mvar <- newTMVarIO dtable_for_encoding
    dtable_for_decoding <- HP.newDynamicTableForDecoding 4096
    dtable_for_decoding_mvar <- newTMVarIO dtable_for_decoding

    waiting_mvar <- newEmptyTMVarIO

    thread_id <- forkIO $ catch
        (do
            attendant attendant_callbacks
        )
        ((\ e -> do
            putStrLn $ "Exception: " ++ (show e)
            maybe_waiting <- atomically $ tryReadTMVar waiting_mvar
            case maybe_waiting of
                Just thread_id -> throwTo thread_id e
                Nothing -> return ()
        ):: SomeException -> IO ())

    remaining_output_bit <- newTMVarIO ""
    let session = DecoySession {
        _inputDataChannel  = input_data_channel,
        _outputDataChannel = output_data_channel,
        _sessionThread     = thread_id,
        _remainingOutputBit= remaining_output_bit,
        _nh2Settings       = NH2.defaultSettings,
        _waiting           = waiting_mvar,
        _encodeHeadersHere = dtable_for_encoding_mvar,
        _decodeHeadersHere = dtable_for_decoding_mvar,
        _incomingData      = incoming_data_tmvar,
        _sessionThrowed    = session_throwed_tmvar
        }
    return session


-- Tell when we are done with a decision
sessionIsUndone :: DecoySession -> IO Bool
sessionIsUndone session = error "NotImplemented"


-- Send a frame to a session
sendFrameToSession :: DecoySession -> (NH2.EncodeInfo, NH2.FramePayload) -> IO ()
sendFrameToSession session (encode_info, frame_payload) = do
    let
        bs_list = NH2.encodeFrameChunks encode_info frame_payload
        input_data_channel = session ^. inputDataChannel

    atomically $
        mapM_
            (writeTChan input_data_channel) bs_list


-- Read a frame from a session... if possible. It will block
-- until the frame comes out, but should fail if there is
-- an exception and/or the session is closed.
recvFrameFromSession :: DecoySession ->  IO (Maybe NH2.Frame)
recvFrameFromSession decoy_session = do
    let
        output_data_channel = decoy_session ^. outputDataChannel
        remaining_output_bit_mvar = decoy_session ^. remainingOutputBit
        pull_action = fmap LB.toStrict $ readTChan output_data_channel
        settings = decoy_session ^. nh2Settings
        waiting_for_read = decoy_session ^. waiting
    my_thread_id <- myThreadId
    packet <- atomically $ do
        putTMVar waiting_for_read my_thread_id
        remaining_output_bit <- takeTMVar remaining_output_bit_mvar
        (packet, rest) <- readNextChunkAndContinue http2FrameLength remaining_output_bit pull_action
        putTMVar remaining_output_bit_mvar rest
        takeTMVar waiting_for_read
        return packet
    let
        error_or_frame = NH2.decodeFrame settings packet
    case error_or_frame of
        Left   _      -> return Nothing
        Right  frame  -> return $ Just frame

-- Reads all frames from a session and assembles a response. Notice that this is a buggy hack,
-- because continuation frames and such are not considered.
-- T
-- readResponseFromSession :: DecoySession -> NH2.StreamId -> IO (Headers, B.ByteString, [NH2.Frame])



-- Encode headers to send to the session
-- TODO: There is an important bug here... we are using the default encoding
-- strategy everywhere
encodeHeadersForSession :: DecoySession -> Headers -> IO B.ByteString
encodeHeadersForSession decoy_session headers =
    do
        let
            mv = decoy_session ^. encodeHeadersHere
        dtable <- atomically $ takeTMVar mv
        (dtable', bs ) <- HP.encodeHeader HP.defaultEncodeStrategy dtable headers
        atomically $ putTMVar mv dtable'
        return bs

-- Decode headers to receive them from the session
decodeHeadersForSession :: DecoySession -> B.ByteString -> IO Headers
decodeHeadersForSession decoy_session bs =  do
    let
        mv = decoy_session ^. decodeHeadersHere
    dtable <- atomically $ takeTMVar mv
    (dtable', headers) <- HP.decodeHeader dtable bs
    atomically $ putTMVar mv dtable'
    return headers

-- Send raw data to a session
sendRawDataToSession :: DecoySession -> B.ByteString -> IO ()
sendRawDataToSession decoy_session data_to_send = do
    let
        input_data_channel = decoy_session ^. inputDataChannel
        waiting_for_write = decoy_session ^. waiting

    thread_id <- myThreadId
    atomically $ do
        putTMVar waiting_for_write thread_id
        writeTChan input_data_channel data_to_send
        takeTMVar waiting_for_write
    return ()


-- Send a headers frame to the session that ends the stream...
performRequestSimple :: DecoySession -> Int -> Headers -> IO ()
performRequestSimple decoy_session stream_id headers = do
    headers_data <- encodeHeadersForSession decoy_session headers
    let
        frame = NH2.HeadersFrame
            Nothing  -- No priority
            headers_data
        frame_data_ei = NH2.EncodeInfo {
            NH2.encodeFlags = NH2.setEndStream . NH2.setEndHeader $ NH2.defaultFlags,
            NH2.encodeStreamId = stream_id,
            NH2.encodePadding = Nothing
            }

    sendFrameToSession decoy_session (frame_data_ei,frame)


-- Read raw data from a session. Normally blocks until data be available.
-- It returns Nothing when the session is to be considered finished.
recvRawDataFromSession :: DecoySession -> IO (Maybe B.ByteString)
recvRawDataFromSession decoy_session = error "NotImplemented"
