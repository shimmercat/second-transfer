-- Session: links frames to streams, and helps in ordering the header frames
-- so that they don't get mixed with header frames from other streams when 
-- resources are being served concurrently.
{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell #-}
module Rede.Http2.Session(
    http2Session
    ,getFrameFromSession
    ,sendFrametoSession

    ,CoherentSession
    ,SessionInput 
    ,SessionOutput 
    ,SessionStartData(..)
    ) where


-- System grade utilities
import           Control.Monad                           (forever)
import           Control.Concurrent                      (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad.IO.Class                  (liftIO)
-- import           Control.Monad.Trans.Class               (lift)
-- import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Reader

-- import           Text.Printf                             (printf)
import           Data.Conduit
import           Data.Conduit.List                       (foldMapM)
import qualified Data.ByteString                         as B
import           Control.Concurrent.MVar
import qualified Data.IntSet                             as NS
import qualified Data.HashTable.IO          as H

import           Control.Lens
-- import           Blaze.ByteString.Builder.ByteString (fromByteString)
-- import qualified Blaze.ByteString.Builder            as Bu


-- No framing layer here... let's use Kazu's Yamamoto library
import qualified Network.HTTP2            as NH2
import qualified Network.HPACK            as HP

-- Logging utilities
-- import           System.Log.Formatter
-- import           System.Log.Handler         (setFormatter)
-- import           System.Log.Handler.Simple
-- import           System.Log.Handler.Syslog
import           System.Log.Logger

-- Imports from other parts of the program
import           Rede.MainLoop.CoherentWorker 
import           Rede.MainLoop.Tokens
import           Rede.Utils                             (unfoldChannelAndSource)


-- Unfortunately the frame encoding API of Network.HTTP2 is a bit difficult to 
-- use :-( 
type OutputFrame = (NH2.EncodeInfo, NH2.FramePayload)
type InputFrame  = NH2.Frame


useChunkLength :: Int 
useChunkLength = 16384


-- Singleton instance used for concurrency
data HeadersSent = HeadersSent 


-- Whatever a worker thread is going to need comes here.... 
-- this is to make refactoring easier, but not strictly needed. 
data WorkerThreadEnvironment = WorkerThreadEnvironment {
    -- What's the header stream id?
    _streamId :: GlobalStreamId

    -- A full block of headers can come here... the mvar in the middle should
    -- be populate to signal end of headers transmission. A thread will be suspended
    -- waiting for that
    , _headersOutput :: Chan (GlobalStreamId, MVar HeadersSent, Headers)

    -- And regular contents can come this way and thus be properly mixed
    -- with everything else.... for now... 
    ,_dataOutput :: Chan (GlobalStreamId, B.ByteString)

    ,_streamsCancelled_WTE :: MVar NS.IntSet

    }

makeLenses ''WorkerThreadEnvironment


-- Basically a couple of channels ... 
type Session = (SessionInput, SessionOutput)


-- From outside, one can only write to this one ... the newtype is to enforce 
--    this.
newtype SessionInput = SessionInput ( Chan (Either SessionInputCommand InputFrame) )
sendFrametoSession :: SessionInput  -> InputFrame -> IO ()
sendFrametoSession (SessionInput chan) frame = writeChan chan $ Right frame


-- From outside, one can only read from this one 
newtype SessionOutput = SessionOutput ( Chan (Either SessionOutputCommand OutputFrame) )
getFrameFromSession :: SessionOutput -> IO (Either SessionOutputCommand OutputFrame) 
getFrameFromSession (SessionOutput chan) = readChan chan


-- Here is how we make a session 
type SessionMaker = SessionStartData -> IO Session



-- Here is how we make a session wrapping a CoherentWorker
type CoherentSession = CoherentWorker -> SessionMaker 


type HashTable k v = H.CuckooHashTable k v


-- Blaze builder could be more proper here... 
type Stream2HeaderBlockFragment = HashTable GlobalStreamId B.ByteString


type WorkerMonad = ReaderT WorkerThreadEnvironment IO 


-- Have to figure out which are these...but I would expect to have things
-- like unexpected aborts here in this type.
data SessionInputCommand = 
    CancelSession_SIC
  deriving Show 


-- temporary
data  SessionOutputCommand = 
    CancelSession_SOC
  deriving Show


-- TODO: Put here information needed for the session to work
data SessionStartData = SessionStartData {
    
    }


makeLenses ''SessionStartData


data PostInputMechanism = PostInputMechanism (Chan (Maybe B.ByteString), InputDataStream)


-- NH2.Frame != Frame
data SessionData = SessionData {
    _sessionInput                :: Chan (Either SessionInputCommand InputFrame)

    -- We need to lock this channel occassionally so that we can order multiple 
    -- header frames properly.... 
    ,_sessionOutput              :: MVar (Chan (Either SessionOutputCommand OutputFrame))

    -- Use to encode 
    ,_toEncodeHeaders            :: MVar HP.DynamicTable
    -- And used to decode
    ,_toDecodeHeaders            :: MVar HP.DynamicTable

    -- Used for decoding the headers
    ,_stream2HeaderBlockFragment :: Stream2HeaderBlockFragment

    -- Used for worker threads... this is actually a pre-filled template
    -- I make copies of it in different contexts, and as needed. 
    ,_forWorkerThread            :: WorkerThreadEnvironment

    ,_coherentWorker             :: CoherentWorker

    -- Some streams may be cancelled 
    ,_streamsCancelled           :: MVar NS.IntSet

    -- Data input mechanism corresponding to some threads
    ,_stream2PostInputMechanism  :: HashTable Int PostInputMechanism 
    }


makeLenses ''SessionData


--                                v- {headers table size comes here!!}
http2Session :: CoherentWorker -> SessionStartData -> IO Session
http2Session coherent_worker _ =   do 
    session_input   <- newChan
    session_output  <- newChan
    session_output_mvar <- newMVar session_output


    -- For incremental construction of headers...
    stream_request_headers <- H.new :: IO Stream2HeaderBlockFragment

    -- Warning: we should find a way of coping with different table sizes.
    decode_headers_table <- HP.newDynamicTableForDecoding 4096
    decode_headers_table_mvar <- newMVar decode_headers_table

    encode_headers_table <- HP.newDynamicTableForEncoding 4096
    encode_headers_table_mvar <- newMVar encode_headers_table

    -- These ones need independent threads taking care of sending stuff
    -- their way... 
    headers_output <- newChan :: IO (Chan (GlobalStreamId, MVar HeadersSent, Headers))
    data_output <- newChan :: IO (Chan (GlobalStreamId,B.ByteString))

    stream2postinputmechanism <- H.new 

    -- What about stream cancellation?
    cancelled_streams_mvar <- newMVar $ NS.empty :: IO (MVar NS.IntSet)

    let for_worker_thread = WorkerThreadEnvironment {
        _streamId = error "NotInitialized"  
        ,_headersOutput = headers_output
        ,_dataOutput = data_output
        ,_streamsCancelled_WTE = cancelled_streams_mvar
        }

    let session_data  = SessionData {
        _sessionInput                = session_input 
        ,_sessionOutput              = session_output_mvar
        ,_toDecodeHeaders            = decode_headers_table_mvar
        ,_toEncodeHeaders            = encode_headers_table_mvar
        ,_stream2HeaderBlockFragment = stream_request_headers
        ,_forWorkerThread            = for_worker_thread
        ,_coherentWorker             = coherent_worker
        ,_streamsCancelled           = cancelled_streams_mvar
        ,_stream2PostInputMechanism  = stream2postinputmechanism
        }

    -- Create an input thread that decodes frames...
    forkIO $ runReaderT sessionInputThread session_data
 
    -- Create a thread that captures headers and sends them down the tube 
    forkIO $ runReaderT (headersOutputThread headers_output session_output_mvar) session_data

    -- Create a thread that captures data and sends it down the tube
    forkIO $ dataOutputThread data_output session_output_mvar

    -- The two previous thread fill the session_output argument below (they write to it)
    -- the session machinery in the other end is in charge of sending that data through the 
    -- socket.
    

    return ( (SessionInput session_input),
             (SessionOutput session_output) )



sessionInputThread :: ReaderT SessionData IO ()
sessionInputThread  = do 
    liftIO $ debugM "HTTP2.Session" "Entering sessionInputThread"

    -- This is an introductory and declarative block... all of this is tail-executed
    -- every time that  a packet needs to be processed. It may be a good idea to abstract
    -- these values in a closure... 
    session_input             <- view sessionInput 
    
    decode_headers_table_mvar <- view toDecodeHeaders 
    stream_request_headers    <- view stream2HeaderBlockFragment
    cancelled_streams_mvar    <- view streamsCancelled
    coherent_worker           <- view coherentWorker

    for_worker_thread_uns     <- view forWorkerThread

    input                     <- liftIO $ readChan session_input

    liftIO $ debugM "HTTP2.Session" $ "Got a frame or a command: " ++ (show input)

    case input of 

        Left _ -> do 
            -- Actually, I haven't even defined a type for these tokens....
            error "Received control token but don't know what to do with it..."

        Right frame | Just (stream_id, bytes) <- frameIsHeaderOfStream frame -> do 
            -- Just append the frames to streamRequestHeaders
            appendHeaderFragmentBlock stream_id bytes

            if frameEndsHeaders frame then 
              do
                -- Let's decode the headers
                let for_worker_thread     = set streamId stream_id for_worker_thread_uns 
                headers_bytes             <- getHeaderBytes stream_id
                dyn_table                 <- liftIO $ takeMVar decode_headers_table_mvar
                (new_table, header_list ) <- liftIO $ HP.decodeHeader dyn_table headers_bytes
                -- Good moment to remove the headers from the table.... we don't want a space
                -- leak here 
                liftIO $ H.delete stream_request_headers stream_id
                liftIO $ putMVar decode_headers_table_mvar new_table

                -- If the headers end the request.... 
                post_data_source <- if not (frameEndsStream frame)
                  then do 
                    
                    mechanism <- createMechanismForStream stream_id 
                    let source = postDataSourceFromMechanism mechanism
                    return $ Just source
                  else do 
                    -- liftIO $ putStrLn "Headers end reqeust"
                    return Nothing


                -- I'm clear to start the worker, in its own thread
                -- !!
                liftIO . forkIO $ runReaderT 
                    (workerThread (header_list, post_data_source) coherent_worker)
                    for_worker_thread 

                return ()
            else 
                -- Frame doesn't end the headers... it was added before... so
                -- probably do nothing 
                return ()
                
            continue 

        Right frame@(NH2.Frame _ (NH2.RSTStreamFrame error_code_id)) -> do
            liftIO $ infoM "HTTP2.Session" $ "Stream reset: " ++ (show error_code_id)
            cancelled_streams <- liftIO $ readMVar cancelled_streams_mvar
            let stream_id = streamIdFromFrame frame
            liftIO $ infoM "HTTP2.Session" $ "Cancelled stream was: " ++ (show stream_id)
            liftIO $ putMVar cancelled_streams_mvar $ NS.insert  stream_id cancelled_streams

            continue 


        Right frame@(NH2.Frame (NH2.FrameHeader _ _ nh2_stream_id) (NH2.DataFrame somebytes)) -> do 
            -- So I got data to process
            -- TODO: Handle end of stream
            let stream_id = NH2.fromStreamIdentifier nh2_stream_id 
            streamWorkerSendData stream_id somebytes
            -- After that data has been received and forwarded downstream, we can issue a windows update
            --
            -- TODO: We can use wider frames to avoid congestion...
            -- .... and we can also be more compositional with these short bursts of data....
            sendOutFrame
                (NH2.EncodeInfo
                    NH2.defaultFlags
                    nh2_stream_id
                    Nothing
                )
                (NH2.WindowUpdateFrame
                    (fromIntegral (B.length somebytes))
                )
            sendOutFrame
                (NH2.EncodeInfo
                    NH2.defaultFlags
                    (NH2.toStreamIdentifier 0)
                    Nothing
                )
                (NH2.WindowUpdateFrame
                    (fromIntegral (B.length somebytes))
                )                

            if frameEndsStream frame  
              then do 
                -- Good place to close the source ... 
                closePostDataSource stream_id 
              else 
                return ()

            continue 


        Right (NH2.Frame (NH2.FrameHeader _ flags _) (NH2.PingFrame _)) | NH2.testAck flags-> do 
            -- Deal with pings: this is an Ack, so do nothing
            continue 

        Right (NH2.Frame (NH2.FrameHeader _ _ _) (NH2.PingFrame somebytes))  -> do 
            -- Deal with pings: NOT an Ack, so answer
            liftIO $ debugM "HTTP2.Session" "Ping processed"
            sendOutFrame
                (NH2.EncodeInfo
                    (NH2.setAck NH2.defaultFlags)
                    (NH2.toStreamIdentifier 0)
                    Nothing 
                )
                (NH2.PingFrame somebytes)

            continue 

        Right (NH2.Frame frame_header (NH2.SettingsFrame _)) | isSettingsAck frame_header -> do 
            -- Frame was received by the peer, do nothing here...
            continue 


        Right (NH2.Frame _ (NH2.SettingsFrame settings_list))  -> do 
            liftIO $ debugM "HTTP2.Session" $ "Received settings: " ++ (show settings_list)
            -- Just acknowledge the frame.... for now 
            sendOutFrame 
                (NH2.EncodeInfo
                    (NH2.setAck NH2.defaultFlags)
                    (NH2.toStreamIdentifier 0)
                    Nothing )
                (NH2.SettingsFrame [])

            continue 


        Right somethingelse -> do 
            liftIO $ errorM "HTTP2.Session" $  "Received problematic frame: "
            liftIO $ errorM "HTTP2.Session" $  "..  " ++ (show somethingelse)

            continue 

  where 

    continue = sessionInputThread

    sendOutFrame :: NH2.EncodeInfo -> NH2.FramePayload -> ReaderT SessionData IO ()
    sendOutFrame encode_info payload = do 
        session_output_mvar <- view sessionOutput 
        session_output <- liftIO $ takeMVar session_output_mvar

        liftIO $ writeChan session_output $ Right (encode_info, payload)

        liftIO $ putMVar session_output_mvar session_output


frameEndsStream :: InputFrame -> Bool 
frameEndsStream (NH2.Frame (NH2.FrameHeader _ flags _) _)  = NH2.testEndStream flags


createMechanismForStream :: GlobalStreamId -> ReaderT SessionData IO PostInputMechanism
createMechanismForStream stream_id = do 
    (chan, source) <- liftIO $ unfoldChannelAndSource
    stream2postinputmechanism <- view stream2PostInputMechanism
    let pim = PostInputMechanism (chan, source)
    liftIO $ H.insert stream2postinputmechanism stream_id pim 
    return pim


-- TODO: Can be optimized by factoring out the mechanism lookup
closePostDataSource :: GlobalStreamId -> ReaderT SessionData IO ()
closePostDataSource stream_id = do 
    stream2postinputmechanism <- view stream2PostInputMechanism

    pim_maybe <- liftIO $ H.lookup stream2postinputmechanism stream_id 

    case pim_maybe of 

        Just (PostInputMechanism (chan, _))  -> 
            liftIO $ writeChan chan Nothing

        Nothing -> 
            -- This is an internal error, the mechanism should be 
            -- created when the stream ends
            error "Internal error/closePostDataSource"


streamWorkerSendData :: Int -> B.ByteString -> ReaderT SessionData IO ()
streamWorkerSendData stream_id bytes = do 
    s2pim <- view stream2PostInputMechanism
    pim_maybe <- liftIO $ H.lookup s2pim stream_id 

    case pim_maybe of 

        Just pim  -> 
            sendBytesToPim pim bytes

        Nothing -> 
            -- This is an internal error, the mechanism should be 
            -- created when the headers end
            error "Internal error"


sendBytesToPim :: PostInputMechanism -> B.ByteString -> ReaderT SessionData IO ()
sendBytesToPim (PostInputMechanism (chan, _)) bytes = 
    liftIO $ writeChan chan (Just bytes)


postDataSourceFromMechanism :: PostInputMechanism -> InputDataStream
postDataSourceFromMechanism (PostInputMechanism (_, source)) = source


isSettingsAck :: NH2.FrameHeader -> Bool 
isSettingsAck (NH2.FrameHeader _ flags _) = 
    NH2.testAck flags


isStreamCancelled :: GlobalStreamId  -> WorkerMonad Bool 
isStreamCancelled stream_id = do 
    cancelled_streams_mvar <- view streamsCancelled_WTE
    cancelled_streams <- liftIO $ readMVar cancelled_streams_mvar
    return $ NS.member stream_id cancelled_streams


workerThread :: Request -> CoherentWorker -> WorkerMonad ()
workerThread req coherent_worker =
  do
    headers_output <- view headersOutput
    stream_id <- view streamId
    (headers, _, data_and_conclussion) <- liftIO $ coherent_worker req

    -- liftIO $ putStrLn $ "Num pushed streams: " ++ (show $ length pushed_streams)

    -- Now I send the headers, if that's possible at all
    headers_sent <- liftIO $ newEmptyMVar
    liftIO $ writeChan headers_output (stream_id, headers_sent, headers)

    -- At this moment I should ask if the stream hasn't been cancelled by the browser before
    -- commiting to the work of sending addtitional data
    is_stream_cancelled <- isStreamCancelled stream_id
    if not is_stream_cancelled

      then 
        -- I have a beautiful source that I can de-construct...
        -- TODO: Optionally pulling data out from a Conduit ....
        -- liftIO ( data_and_conclussion $$ (_sendDataOfStream stream_id) )
        -- 
        -- This threadlet should block here waiting for the headers to finish going
        (transPipe liftIO data_and_conclussion) $$ (sendDataOfStream stream_id headers_sent)
      else 

        return ()

--                                                       v-- comp. monad.
sendDataOfStream :: GlobalStreamId -> MVar HeadersSent -> Sink B.ByteString (ReaderT WorkerThreadEnvironment IO) ()
sendDataOfStream stream_id headers_sent = do
    data_output <- view dataOutput
    transPipe liftIO $ do 
        -- Wait for permission to send the data
        liftIO $ takeMVar headers_sent
        foldMapM $ \ bytes ->
            writeChan data_output (stream_id, bytes)


-- sendDataOfStream :: Sink     


-- Allow this very important function to be used in the future to process footers

-- difficultFunction :: (Monad m)
--                   => ConduitM () a2 m r1 -> ConduitM a2 Void m r2
--                   -> m (r2, Maybe r1)
-- difficultFunction l r = liftM (fmap getLast) $ runWriterT (l' $$ r')
--   where
--     l' = transPipe lift l >>= lift . tell . Last . Just
--     r' = transPipe lift r



appendHeaderFragmentBlock :: GlobalStreamId -> B.ByteString -> ReaderT SessionData IO ()
appendHeaderFragmentBlock global_stream_id bytes = do 
    ht <- view stream2HeaderBlockFragment 
    maybe_old_block <- liftIO $ H.lookup ht global_stream_id
    new_block <- return $ case maybe_old_block of 

        Nothing -> bytes

        Just something -> something `B.append` bytes 

    liftIO $ H.insert ht global_stream_id new_block


getHeaderBytes :: GlobalStreamId -> ReaderT SessionData IO B.ByteString
getHeaderBytes global_stream_id = do 
    ht <- view stream2HeaderBlockFragment 
    Just bytes <- liftIO $ H.lookup ht global_stream_id
    return bytes


frameIsHeaderOfStream :: InputFrame -> Maybe (GlobalStreamId, B.ByteString)
frameIsHeaderOfStream (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.HeadersFrame _ block_fragment   ) )
    = Just (NH2.fromStreamIdentifier stream_id, block_fragment)
frameIsHeaderOfStream (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.ContinuationFrame block_fragment) )
    = Just (NH2.fromStreamIdentifier stream_id, block_fragment)
frameIsHeaderOfStream _                                       
    = Nothing 


frameEndsHeaders  :: InputFrame -> Bool 
frameEndsHeaders (NH2.Frame (NH2.FrameHeader _ flags _) _) = NH2.testEndHeader flags


streamIdFromFrame :: InputFrame -> GlobalStreamId
streamIdFromFrame (NH2.Frame (NH2.FrameHeader _ _ stream_id) _) = NH2.fromStreamIdentifier stream_id


-- TODO: Have different size for the headers..... just now going with a default size of 16 k...
-- TODO: Find a way to kill this thread....
headersOutputThread :: Chan (GlobalStreamId, MVar HeadersSent, Headers)
                       -> MVar (Chan (Either SessionOutputCommand OutputFrame)) 
                       -> ReaderT SessionData IO ()
headersOutputThread input_chan session_output_mvar = forever $ do 
    (stream_id, headers_ready_mvar, headers) <- liftIO $ readChan input_chan
    -- liftIO $ putStrLn $ "Output headers: " ++ (show headers)

    -- First encode the headers using the table
    encode_dyn_table_mvar <- view toEncodeHeaders

    encode_dyn_table <- liftIO $ takeMVar encode_dyn_table_mvar
    (new_dyn_table, data_to_send ) <- liftIO $ HP.encodeHeader HP.defaultEncodeStrategy encode_dyn_table headers
    liftIO $ putMVar encode_dyn_table_mvar new_dyn_table

    -- Now split the bytestring in chunks of the needed size.... 
    let bs_chunks = bytestringChunk useChunkLength data_to_send

    -- And send the chunks through while locking the output place....
    session_output <- liftIO $ takeMVar session_output_mvar

    -- First frame is just a headers frame....
    if (length bs_chunks) == 1 
      then
        do 
            let flags = NH2.setEndHeader NH2.defaultFlags

            -- Write the first frame 
            liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                NH2.encodeFlags     = flags
                ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                ,NH2.encodePadding  = Nothing }, 

                NH2.HeadersFrame Nothing (head bs_chunks)
                )
      else 
        do 
            let flags = NH2.defaultFlags
            -- Write the first frame 
            liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                NH2.encodeFlags     = flags
                ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                ,NH2.encodePadding  = Nothing }, 

                NH2.HeadersFrame Nothing (head bs_chunks)
                )
            -- And write the other frames
            let 
                writeContinuations :: [B.ByteString] -> ReaderT SessionData IO ()
                writeContinuations (last_fragment:[]) = liftIO $
                    writeChan session_output $ Right ( NH2.EncodeInfo {
                        NH2.encodeFlags     = NH2.setEndHeader NH2.defaultFlags 
                        ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                        ,NH2.encodePadding  = Nothing }, 

                        NH2.ContinuationFrame last_fragment
                        )
                writeContinuations (fragment:xs) = do 
                    liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                        NH2.encodeFlags     = NH2.defaultFlags 
                        ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                        ,NH2.encodePadding  = Nothing }, 

                        NH2.ContinuationFrame fragment
                        )
                    writeContinuations xs


            writeContinuations (tail bs_chunks)
    -- Restore output capability, so that other pieces waiting can send...
    liftIO $ putMVar session_output_mvar session_output
    -- And say that the headers for this thread are out 
    liftIO $ putMVar headers_ready_mvar HeadersSent
    -- liftIO $ putStrLn "Headers were output"
  

bytestringChunk :: Int -> B.ByteString -> [B.ByteString]
bytestringChunk len s | (B.length s) < len = [ s ]
bytestringChunk len s = h:(bytestringChunk len xs)
  where 
    (h, xs) = B.splitAt len s 


-- TODO: find a clean way to finish this thread (maybe with negative stream ids?)
-- TODO: This function does non-optimal chunking for the case where responses are
--       actually streamed.... in those cases we need to keep state for frames in 
--       some other format.... 
dataOutputThread :: Chan (GlobalStreamId, B.ByteString)
                    -> MVar (Chan (Either SessionOutputCommand OutputFrame)) 
                    -> IO ()
dataOutputThread input_chan session_output_mvar = forever $ do 
    (stream_id, contents) <- readChan input_chan

    -- And now just simply output it...
    let bs_chunks = bytestringChunk useChunkLength contents
    -- putStrLn $ "Chunk lengths: " ++ (show (map B.length bs_chunks))

    -- And send the chunks through while locking the output place....
    session_output <- liftIO $ takeMVar session_output_mvar

    -- First frame is the only one:
    if (length bs_chunks) == 1 
      then
        do 
            let flags = NH2.setEndStream NH2.defaultFlags

            -- Write the first frame 
            liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                NH2.encodeFlags     = flags
                ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                ,NH2.encodePadding  = Nothing }, 

                NH2.DataFrame (head bs_chunks)
                )
      else 
        do 
            let flags = NH2.defaultFlags
            -- Write the first frame 
            liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                NH2.encodeFlags     = flags
                ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                ,NH2.encodePadding  = Nothing }, 

                NH2.DataFrame (head bs_chunks)
                )
            -- And write the other frames
            let 
                writeContinuations :: [B.ByteString] -> IO ()
                writeContinuations (last_fragment:[]) = liftIO $
                    writeChan session_output $ Right ( NH2.EncodeInfo {
                        NH2.encodeFlags     = NH2.setEndStream NH2.defaultFlags 
                        ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                        ,NH2.encodePadding  = Nothing }, 

                        NH2.DataFrame last_fragment
                        )
                writeContinuations (fragment:xs) = do 
                    liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                        NH2.encodeFlags     = NH2.defaultFlags 
                        ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                        ,NH2.encodePadding  = Nothing }, 

                        NH2.DataFrame fragment
                        )
                    writeContinuations xs
            writeContinuations (tail bs_chunks)
    -- Restore output capability, so that other pieces waiting can send...
    liftIO $ debugM "HTTP2.Session" $  "Output capability restored"
    liftIO $ putMVar session_output_mvar session_output                    