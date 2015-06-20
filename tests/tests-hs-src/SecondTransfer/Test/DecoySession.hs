{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module SecondTransfer.Test.DecoySession where 



import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Lens            (view, (^.))
import qualified Control.Lens            as L

import qualified Network.HTTP2           as NH2
import qualified Network.HPACK           as HP

import qualified Data.ByteString         as B 
import qualified Data.ByteString.Lazy    as LB

import           SecondTransfer.Http2
import           SecondTransfer.Types
import           SecondTransfer.MainLoop.Internal


data DecoySession = DecoySession {
    _inputDataChannel   :: Chan B.ByteString
    ,_outputDataChannel :: Chan LB.ByteString
    ,_sessionThread     :: ThreadId
    ,_remainingOutputBit:: MVar B.ByteString
    ,_nh2Settings       :: NH2.Settings
    ,_sessionThrowed    :: MVar Bool
    ,_waiting           :: MVar ThreadId 
    ,_encodeHeadersHere :: MVar HP.DynamicTable
    ,_decodeHeadersHere :: MVar HP.DynamicTable
    }


L.makeLenses ''DecoySession


-- Supposed to be an HTTP/2 attendant
createDecoySession :: Attendant -> IO DecoySession
createDecoySession attendant = do
    input_data_channel  <- newChan 
    output_data_channel <- newChan
    let 
        push_action  :: PushAction
        push_action = writeChan output_data_channel
        pull_action  :: PullAction 
        pull_action = readChan input_data_channel
        close_action :: CloseAction 
        close_action = do
            return ()

    dtable_for_encoding <- HP.newDynamicTableForEncoding 4096
    dtable_for_encoding_mvar <- newMVar dtable_for_encoding
    dtable_for_decoding <- HP.newDynamicTableForDecoding 4096
    dtable_for_decoding_mvar <- newMVar dtable_for_decoding

    waiting_mvar <- newEmptyMVar
        
    thread_id <- forkIO $ catch 
        (do 
            attendant push_action pull_action close_action
        )
        ((\ e -> do 
            putStrLn $ "Exception: " ++ (show e)
            maybe_waiting <- tryTakeMVar waiting_mvar
            case maybe_waiting of 
                Just thread_id -> throwTo thread_id e
                Nothing -> return ()
        ):: SomeException -> IO ())

    remaining_output_bit <- newMVar ""
    let session = DecoySession {
        _inputDataChannel  = input_data_channel,
        _outputDataChannel = output_data_channel,
        _sessionThread     = thread_id,
        _remainingOutputBit= remaining_output_bit,
        _nh2Settings       = NH2.defaultSettings,
        _waiting           = waiting_mvar,
        _encodeHeadersHere = dtable_for_encoding_mvar,
        _decodeHeadersHere = dtable_for_decoding_mvar
        }
    return session


-- Tell when we are done with a decision
sessionIsUndone :: DecoySession -> IO Bool
sessionIsUndone session = error "NotImplemented"


-- Send a frame to a session 
sendFrameToSession :: DecoySession -> OutputFrame -> IO ()
sendFrameToSession session (encode_info, frame_payload) = do 
    let 
        bs_list = NH2.encodeFrameChunks encode_info frame_payload
        input_data_channel = session ^. inputDataChannel

    mapM_ (\ x -> writeChan input_data_channel x ) bs_list


-- Read a frame from a session... if possible. It will block 
-- until the frame comes out, but should fail if there is 
-- an exception and/or the session is closed.
recvFrameFromSession :: DecoySession ->  IO (Maybe NH2.Frame)
recvFrameFromSession decoy_session = do
    let 
        output_data_channel = decoy_session ^. outputDataChannel
        remaining_output_bit_mvar = decoy_session ^. remainingOutputBit
        pull_action = fmap LB.toStrict $ readChan output_data_channel 
        settings = decoy_session ^. nh2Settings
        waiting_for_read = decoy_session ^. waiting
    my_thread_id <- myThreadId
    putMVar waiting_for_read my_thread_id
    remaining_output_bit <- takeMVar remaining_output_bit_mvar
    (packet, rest) <- readNextChunkAndContinue http2FrameLength remaining_output_bit pull_action
    putMVar remaining_output_bit_mvar rest
    takeMVar waiting_for_read
    let 
        error_or_frame = NH2.decodeFrame settings packet
    case error_or_frame of 
        Left   _      -> return Nothing 
        Right  frame  -> return $ Just frame


-- Encode headers to send to the session 
-- TODO: There is an important bug here... we are using the default encoding
-- strategy everywhere
encodeHeadersForSession :: DecoySession -> Headers -> IO B.ByteString
encodeHeadersForSession decoy_session headers = 
  do 
    let 
        mv = (decoy_session ^. encodeHeadersHere )
    dtable <- takeMVar mv
    (dtable', bs ) <- HP.encodeHeader HP.defaultEncodeStrategy dtable headers 
    putMVar mv dtable'
    return bs

-- Decode headers to receive them from the session 
decodeHeadersForSession :: DecoySession -> B.ByteString -> IO Headers 
decodeHeadersForSession decoy_session bs = 
  do 
    let 
        mv = (decoy_session ^. decodeHeadersHere )
    dtable <- takeMVar mv 
    (dtable', headers) <- HP.decodeHeader dtable bs
    putMVar mv dtable'
    return headers

-- Send raw data to a session 
sendRawDataToSession :: DecoySession -> B.ByteString -> IO ()
sendRawDataToSession decoy_session data_to_send = do
    let 
        input_data_channel = decoy_session ^. inputDataChannel
        waiting_for_write = decoy_session ^. waiting

    thread_id <- myThreadId
    putMVar waiting_for_write thread_id 
    writeChan input_data_channel data_to_send
    takeMVar waiting_for_write
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
            NH2.encodeStreamId = NH2.toStreamIdentifier stream_id,
            NH2.encodePadding = Nothing
            }
    
    sendFrameToSession decoy_session (frame_data_ei,frame)


-- Read raw data from a session. Normally blocks until data be available.
-- It returns Nothing when the session is to be considered finished. 
recvRawDataFromSession :: DecoySession -> IO (Maybe B.ByteString)
recvRawDataFromSession decoy_session = error "NotImplemented"