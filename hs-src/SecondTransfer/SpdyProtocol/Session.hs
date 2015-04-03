{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module SecondTransfer.SpdyProtocol.Session(
    -- trivialSession
    basicSession
    -- ,showHeadersIfPresent
    ) where


import           Control.Concurrent                      (forkIO)
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Reader
import           Control.Exception(throwIO)

import           Data.Conduit
-- import           Data.IORef
import qualified Data.Streaming.Zlib                     as Z

-- import           Data.Conduit.Lift                       (distribute)
-- import qualified Data.Conduit.List                       as CL
import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                    as LB
import           Data.ByteString.Char8                   (pack)
import           Data.Default                            (def)
import           Control.Concurrent.MVar
import qualified Data.Map                                as MA
import           Data.Binary.Put                         (runPut)
import qualified Data.Binary                             as Bi
import           Data.Binary.Get                         (runGet)
import qualified Data.HashTable.IO          as H
import qualified Data.Dequeue               as D


import           SecondTransfer.SpdyProtocol.Framing.AnyFrame
import           SecondTransfer.SpdyProtocol.Framing.Frame
import           SecondTransfer.SpdyProtocol.Framing.Ping
import           SecondTransfer.SpdyProtocol.Framing.DataFrame
import           SecondTransfer.SpdyProtocol.Framing.RstStream
import           SecondTransfer.SpdyProtocol.Framing.WindowUpdate
import           SecondTransfer.MainLoop.StreamPlug
import           SecondTransfer.SpdyProtocol.Framing.KeyValueBlock
import qualified SecondTransfer.SpdyProtocol.Framing.Settings      as SeF
import           SecondTransfer.SpdyProtocol.Streams.State 
import           SecondTransfer.MainLoop.Tokens


-- TODO: Move to constants?


initialSettings :: SeF.SettingsFrame
initialSettings = SeF.SettingsFrame {
     SeF.prologue = def 
    ,SeF.persistSettings = [
        (SeF.InitialWindowSize_S, 65536, SeF.None_PS)
    ]
}


-- goAwayMsg :: GoA.GoAwayFrame
-- goAwayMsg = GoA.GoAwayFrame {
--      GoA.prologue = def 
--     ,GoA.statusCode = GoA.OK_GAR
--     ,GoA.lastGoodStream = 0
-- }


type WaitList = D.BankersDequeue AnyFrame 


-- A table from stream id to windows reamining list and waiting 
-- list
type StreamWindowInfo = MVar (Int, WaitList)
type StreamWaits = H.BasicHashTable Int StreamWindowInfo

data SimpleSessionStateRecord = SimpleSessionStateRecord {
    streamInputs :: MVar (MA.Map Int (MVar  AnyFrame)) 
    -- ,streamsOutput :: MVar (Maybe AnyFrame)

    ,sendZLib     :: MVar Z.Deflate
    ,recvZLib     :: MVar Z.Inflate
    ,streamInit   :: Int -> IO () -> StreamStateT IO () -> IO ()

    -- Need also a way to do frame-flow control 
    ,streamWaits  :: StreamWaits

    ,initialWindowSize :: MVar Int

    ,sessionWindow :: MVar Int
}


type SessionM = ReaderT SimpleSessionStateRecord 
 
-- | Super-simple session manager without flow control and such.... 
-- but using StreamWorkers already....
-- TODO: without proper flow control, we are in troubles....
basicSession :: (StreamWorkerClass  serviceParams servicePocket sessionPocket) =>
  servicePocket -> IO ( (Sink AnyFrame IO () ), (Source IO AnyFrame ) )
basicSession worker_service_pocket = do

    -- Create the input record.... 
    stream_inputs         <- newMVar $ MA.empty

    -- Whatever is put here makes it to the socket, so this
    -- should go after flow control
    session_gate          <- (newEmptyMVar :: IO (MVar AnyFrame) )

    send_zlib             <- Z.initDeflateWithDictionary 2 zLibInitDict Z.defaultWindowBits
    send_zlib_mvar        <- newMVar send_zlib
    recv_zlib             <- Z.initInflateWithDictionary Z.defaultWindowBits zLibInitDict
    recv_zlib_mvar        <- newMVar recv_zlib
    worker_session_pocket <- initSession worker_service_pocket
    -- Preserve the IO wrapper
    make_worker           <- return $ initStream worker_service_pocket worker_session_pocket
    next_stream_id        <- newMVar 2 -- Stream ids pushed from the server start at two
    stream_waits          <- H.new

    -- TODO: Fix this
    initial_window_size   <- newMVar 65536

    session_window        <- newMVar 65536

    session_record    <- return $ SimpleSessionStateRecord {
        streamInputs       = stream_inputs
        ,sendZLib          = send_zlib_mvar
        ,recvZLib          = recv_zlib_mvar
        ,streamInit        = \ stream_id fin -> initStreamState 
                                                stream_id 
                                                fin 
                                                next_stream_id 
        ,streamWaits       = stream_waits
        ,initialWindowSize = initial_window_size
        ,sessionWindow     = session_window
        }
 
    -- hoister  <- return $ (\ x -> runReaderT x session_record :: SessionM IO a -> IO a)

    flow_control_gate <- liftIO $ newEmptyMVar

    -- This is the end of the pipeline which is closest to the TCP socket in the input direction
    -- The "flow_control_gate" variable here is given to newly created streams, it is also used 
    -- directly by the sink to process WindowUpdate frames... 
    packet_sink <- return $ transPipe ( \ x -> runReaderT x session_record)  (statefulSink make_worker flow_control_gate)

    -- We will need to run flow control
    liftIO $ forkIO $ runReaderT (flowControl flow_control_gate session_gate) session_record

    -- This is the end of the pipeline that is closest to the TCP socket in the output direction
    packet_source <- return $ transPipe (\ x -> runReaderT x session_record)  (createTrivialSource session_gate)
    
    return (packet_sink, packet_source)


takesInput :: MVar AnyFrame ->  Source (StreamStateT IO) AnyFrame
takesInput input_mvar = do 
    frame <- liftIO $ takeMVar input_mvar
    -- liftIO $ putStrLn "Got something"
    yield frame 
    takesInput input_mvar


plugStream :: 
    IO StreamWorker ->
    MVar AnyFrame -> 
    MVar  (Either AnyFrame (Int,Int) ) -> 
    IO ( StreamStateT IO () )
plugStream 
    workerStart
    input_mvar 
    drop_output_here_mvar = do 
        worker <-  workerStart
        return  (( (takesInput input_mvar)  $= inputPlug 
            =$= (transPipe liftIO (worker::StreamWorker)) 
            =$= (outputPlug :: Conduit StreamOutputAction (StreamStateT IO) AnyFrame)
            -- ATTENTION: potential session race-condition here. 
            $$  (streamOutput drop_output_here_mvar) ))


-- | Takes output from the stream conduit and puts it on the output mvar. This runs in 
--   the stream thread. 
--   ATTENTION: When a stream finishes, the conduit closes and yields a  Nothing to 
--   signal that
streamOutput :: MVar (Either AnyFrame (Int,Int) ) -> Sink AnyFrame (StreamStateT IO) () 
streamOutput  output_mvar  = do 
    any_frame_maybe <- await 
    case any_frame_maybe of  

        -- The stream is alive
        Just anyframe -> do
            liftIO $ putMVar output_mvar $ Left anyframe
            streamOutput output_mvar

        --The stream wishes to finish, in this case, 
        --don't put anything in the output MVar, but 
        --call a provided finalizer
        Nothing  -> do
            lift streamFinalize
            -- Now let natural finalization of the conduit to take 
            -- place...



-- IMPLEMENT: iDropThisFrame  (Ping_CFT) 

iDropThisFrame :: AnyControlFrame -> Bool
-- iDropThisFrame  (SettingsFrame_ACF     _ )    = True 
-- iDropThisFrame  (WindowUpdateFrame_ACF _ )    = True
iDropThisFrame  _                             = False


-- | Takes a stream worker constructor and properly sets its connections so that 
--   it can take and place data in the multi-threaded pipeline.
statefulSink :: 
    IO StreamWorker                        -- ^ When needed, create a new stream worker here.
    -> MVar (Either AnyFrame (Int,Int) )   -- ^ All outputs of this session should be placed here
                                           --   (This should go to )
    -> Sink AnyFrame (SessionM IO) ()      
statefulSink  init_worker flow_control_gate  = do 

    anyframe_maybe <- await 
    session_record <- lift $ ask
    stream_init    <- return $ streamInit session_record 
    -- session_window <- return $ sessionWindow session_record


    case anyframe_maybe of 

        Just anyframe -> do  

            headers_uncompressed <- lift $ uncompressFrameHeaders anyframe

            case headers_uncompressed of 

                (AnyControl_AF control_frame)  | iDropThisFrame control_frame -> do 
                    liftIO $ putStrLn $ "Frame dropped: " ++ (show control_frame)
                    continue -- ##

                (AnyControl_AF (WindowUpdateFrame_ACF winupdate) ) -> do 
                    stream_id   <- return $ streamIdFromFrame winupdate
                    delta_bytes <- return $ deltaWindowSize winupdate
                    -- liftIO $ putStrLn $ "Window update stream=" ++ (show stream_id) ++ " delta=" ++ (show delta_bytes)
                    liftIO $ putMVar flow_control_gate $ Right (stream_id, delta_bytes)
                    continue

                -- We started here: sending ping requests.... often we also 
                -- need to answer to them...
                (AnyControl_AF (PingFrame_ACF ping_frame)) -> do
                    case handlePingFrame ping_frame of 
                        Just  answer ->
                            liftIO $ putMVar flow_control_gate $ Left $ wrapCF answer 
                        Nothing      ->
                            return ()
                    continue -- ##


                frame@(AnyControl_AF (SynStream_ACF syn_stream)) ->  let 
                        stream_id = streamIdFromFrame syn_stream
                        inputs = streamInputs session_record
                        fin = do 
                            stream_inputs <- takeMVar inputs
                            putMVar inputs $ MA.delete stream_id stream_inputs 
                        stream_create = do 
                            putStrLn $ "Opening stream " ++ (show stream_id)
                            stream_inputs <- takeMVar inputs
                            input_place   <- newEmptyMVar
                            stream_worker <- plugStream init_worker input_place flow_control_gate
                            putMVar inputs $ MA.insert stream_id input_place stream_inputs
                            forkIO $ stream_init stream_id fin $ stream_worker
                            putMVar input_place frame    
                    in do 
                        liftIO stream_create 
                        continue -- ##


                ( AnyControl_AF (RstStreamFrame_ACF rst_stream) )  -> do 
                        stream_id <- return $ streamIdFromFrame rst_stream
                        liftIO $ putStrLn $ "Reset stream " ++ (show stream_id) ++ " because: "++ (show $ getFrameResetReason rst_stream)
                        lift $ deleteStream stream_id
                        continue -- ##


                (AnyControl_AF (GoAwayFrame_ACF goaway)) -> do 
                        -- To test: the socket should be closed here
                        liftIO $ putStrLn $ "GOAWAY (closing this sink) " ++ (show goaway)
                        -- Don't continue here

                (AnyControl_AF (SettingsFrame_ACF settings)) -> do 

                        case SeF.getDefaultWindowSize settings of  

                            Just sz           -> do 
                                liftIO $ putStrLn $ "Settings window size: " ++ (show sz)
                                liftIO $ modifyMVar_ (initialWindowSize session_record)

                                                     (\ _ -> return  sz)

                            Nothing            -> 
                                return ()

                        continue -- ##

                frame -> do 
                        liftIO $ putStrLn  $ "Dont't know how to handle ... " ++ (show frame)
                        continue -- ##
            
            -- Come and recurse... 
           
        Nothing -> return ()  -- So must one finish here...
  where 
    continue =  statefulSink init_worker flow_control_gate



handlePingFrame :: PingFrame -> Maybe PingFrame
handlePingFrame p@(PingFrame _ frame_id) |  odd frame_id = Just p
handlePingFrame _ = Nothing  


addBytesToStream :: Int -> Int -> SessionM IO ()
addBytesToStream stream_id bytecount = do 
    stream_waits <- asks streamWaits
    if stream_id > 0
      then do
        stream_window_info_maybe <- liftIO $ H.lookup stream_waits stream_id 
        case stream_window_info_maybe of  

            Nothing -> do 
                -- The stream was possibly deleted before, do nothing...
                return ()
     

            Just mvar -> do 
                -- Modify it 
                liftIO $ modifyMVar_ mvar $ \ (bytes, deq) -> return (bytes+bytecount, deq)
      else do

        -- Surely this refers to the entire thing
        session_window_mvar <- asks sessionWindow
        liftIO $ modifyMVar_ session_window_mvar (\ current_value -> return (current_value + bytecount))
     

initFlowControlOnStream :: Int -> SessionM IO ()
initFlowControlOnStream stream_id = do
    stream_waits          <- asks streamWaits
    (window_bytes, queue) <- createStreamWindowInfo stream_id
    mvar                  <- liftIO $ newMVar (window_bytes , queue)
    liftIO $ H.insert stream_waits stream_id mvar 


createStreamWindowInfo ::  Int -> SessionM IO (Int, WaitList)
createStreamWindowInfo stream_id = do 
    if odd stream_id 
      then do  
        initial_size_ioref <- asks initialWindowSize
        sz                 <- liftIO $ readMVar initial_size_ioref
        return (sz, D.empty)
      else do 
        -- Working around bug on Firefox 
        return (10000000, D.empty)



flowControlAllowsFrame :: Int -> AnyFrame -> SessionM IO Bool
flowControlAllowsFrame stream_id anyframe = do 
    stream_waits <- asks streamWaits
    stream_info_mvar_maybe <- liftIO $ H.lookup stream_waits stream_id

    case stream_info_mvar_maybe of 

        Just stream_info_mvar    -> do
            (bytes_remaining, dq) <- liftIO $ takeMVar stream_info_mvar
            size_to_send <- return $ associatedLength anyframe   
            available_in_session_mv    <- asks sessionWindow
            available_in_session       <- liftIO $ takeMVar available_in_session_mv
            -- liftIO $ putStrLn $ "Bytes remaining stream id " ++ (show stream_id ) ++ " " ++ (show bytes_remaining)

            case ( ((D.length dq) == 0), ((bytes_remaining >= size_to_send) && (size_to_send <= available_in_session) ) ) of 

                (True, True)    ->  do
                    -- Empty dequeue, enough bytes 
                    liftIO $  do 
                        putMVar stream_info_mvar (bytes_remaining-size_to_send, dq)
                        putMVar available_in_session_mv (available_in_session - size_to_send)
                    return True 
                  
                _               ->  do 
                    liftIO $ do 
                        putStrLn $ "FRAME of stream " ++ (show stream_id) ++ " DELAYED rem bytes: " ++ (show bytes_remaining) ++ " size_to_send: " ++ (show size_to_send)
                        putMVar stream_info_mvar (bytes_remaining, (D.pushBack dq anyframe))
                        putMVar available_in_session_mv available_in_session
                    return False

        Nothing                ->   
            -- Stream has been deleted 
            return False



flowControlCanPopFrame :: Int -> SessionM IO (Maybe AnyFrame)
flowControlCanPopFrame stream_id = do 
    session_record             <- ask
    stream_waits               <- asks streamWaits

    if stream_id > 0 
      then do 
        stream_info_mvar_maybe     <- liftIO $ H.lookup stream_waits stream_id

        case stream_info_mvar_maybe of 

            Just stream_info_mvar  -> do

                (bytes_remaining, dq) <- liftIO $ takeMVar stream_info_mvar
                (anyframe_maybe, newqueue) <- return $ D.popFront dq 
                available_in_session_mv    <- asks sessionWindow
                available_in_session       <- liftIO $ takeMVar available_in_session_mv

                (result, newbytes, dq', new_available_in_session)  <- case anyframe_maybe of 

                    Just anyframe   ->
                        if (sz <= bytes_remaining) && (sz <= available_in_session)
                          then do
                            liftIO $ putStrLn $ "Frame of  " ++ (show stream_id) ++  " popped!"
                            return ( (Just anyframe), bytes_remaining - sz, newqueue, available_in_session - sz) 
                          else do
                            liftIO $ putStrLn $ "Impossible: stream_id= " ++ 
                                                         (show stream_id) ++ 
                                                         " stream bytes avail: " ++ 
                                                         (show bytes_remaining) ++ 
                                                         " session avail: " ++ (show available_in_session)

                            return ( Nothing, bytes_remaining, dq, available_in_session)
                      where  
                        sz = associatedLength anyframe 

                    Nothing  -> 
                        return (Nothing, bytes_remaining, dq, available_in_session)

                liftIO $ do 
                    putMVar stream_info_mvar (newbytes, dq')
                    putMVar available_in_session_mv new_available_in_session

                return result 

            Nothing              -> do 
                return Nothing
      else 
        do
            -- More complicated case of the session window, go through all the active streams
            liftIO $ H.foldM  
                (\ p (stream_id', _) ->  runReaderT (innerFold p stream_id') session_record)
                Nothing
                stream_waits
          where 
            innerFold p stream_id' = do 
                -- liftIO $ putStrLn $ "iter: " ++ (show stream_id')

                case p of 

                    Just _  -> return p 

                    Nothing ->  do
                        anyframe_maybe <- flowControlCanPopFrame stream_id'
                        case anyframe_maybe of 
                            Just _       -> do 
                                return anyframe_maybe  
                            Nothing      -> do 
                                return Nothing  



-- This one sits on the output stream, checking to see if frames are allowed to leave
flowControl
    :: MVar (Either AnyFrame (Int,Int) )     -- Input frames and window's 
                                                -- updates come this  way
               -> MVar AnyFrame                 -- Can output frames go this way
               -> SessionM IO ()
flowControl input output  = do

    event <- liftIO $ takeMVar input 

    case event of 

        Left anyframe    ->     if frameIsFlowControlled anyframe
          then do
            stream_id <- return $ streamIdFromAnyFrame anyframe
            can_send  <- flowControlAllowsFrame stream_id anyframe
            if can_send 
              then do
                -- No need to wait
                liftIO $ putMVar output anyframe

              else do
                return ()
          else do
            -- Not flow-controlled, this is most likely headers and synreplies 
            -- generated here in the server, don't obstruct them 
            liftIO $ putMVar output anyframe  

            -- Some of the non-flow controlled frames create send windows when 
            -- they go out, let's take care of that 

            case anyframe of 

                (AnyControl_AF (SynStream_ACF frame) )      -> initFlowControlOnStream $ streamIdFromFrame frame 
                (AnyControl_AF (SynReplyFrame_ACF frame))   -> initFlowControlOnStream $ streamIdFromFrame frame

                _                                           -> return ()


        Right (stream_id, window_delta_size)   -> do 

            -- liftIO $ putStrLn $ "Add to stream: " ++ (show stream_id)
            -- So, I got some more room to send frames
            -- Notice that this may happen before this fragment ever observes 
            -- a frame on that stream, so we need to be sure it exists...
            addBytesToStream stream_id window_delta_size

            sendFramesForStream stream_id output

    -- Tail-recursively invoke
    flowControl input output 

  where 
    sendFramesForStream stream_id output' = do 
        popped_frame_maybe <- flowControlCanPopFrame stream_id 
        case popped_frame_maybe of 

            Just anyframe   -> do
                liftIO $ putMVar output' anyframe
                sendFramesForStream stream_id output'

            Nothing         -> 
                return ()


associatedLength :: AnyFrame -> Int
associatedLength (DataFrame_AF dataframe ) = B.length $ payload dataframe


-- Here is where frames pop-up coming from the individual stream 
-- threads. So, the frames are serialized at this point and any 
-- incoming Key-value block compressed. This runs on the output 
-- thread. The parameter output_mvar is a gate that flow control 
-- uses to put frames which are ready for delivery...
createTrivialSource :: MVar AnyFrame -> Source (SessionM IO) AnyFrame
createTrivialSource output_mvar = do 
    yield $ wrapCF initialSettings
    createTrivialSourceLoop :: Source (SessionM IO) AnyFrame
  where 
    createTrivialSourceLoop = do

        anyframe_headerscompressed <- lift $ do 
            anyframe <- liftIO $ takeMVar output_mvar

            compressFrameHeaders anyframe

        -- This is a good place to remove the entry from the table if 
        -- this frame is the last one in the stream 
        case frameEndsStream anyframe_headerscompressed of 

            Just which_stream -> do
                liftIO $ putStrLn $ "Stream " ++ (show which_stream) ++  " closed naturally"
                lift $ deleteStream which_stream
        
            Nothing         -> 
                return ()

        liftIO $ putStrLn $ "SENDING of: " ++ (show $ streamIdFromAnyFrame anyframe_headerscompressed)
        yield anyframe_headerscompressed
        createTrivialSourceLoop


deleteStream :: Int -> SessionM IO ()
deleteStream stream_id = do 
    stream_waits <- asks streamWaits
    liftIO $ H.delete stream_waits stream_id


-- | Use to purge old streams from tables....
frameEndsStream :: AnyFrame -> Maybe Int
frameEndsStream (DataFrame_AF dataframe) =
    if has_fin_flag then Just (streamIdFromFrame dataframe) else Nothing
  where 
    has_fin_flag = getFrameFlag dataframe Fin_F
frameEndsStream _ = Nothing


frameIsFlowControlled :: AnyFrame -> Bool 
frameIsFlowControlled (DataFrame_AF _)   = True 
frameIsFlowControlled _                  = False 


compressFrameHeaders :: AnyFrame -> (SessionM IO) AnyFrame 
compressFrameHeaders ( AnyControl_AF (SynStream_ACF f))      = do 
    new_frame <- justCompress f
    return $ wrapCF new_frame 
compressFrameHeaders ( AnyControl_AF (SynReplyFrame_ACF f )) = do 
    new_frame <- justCompress f
    return $ wrapCF new_frame 
compressFrameHeaders ( AnyControl_AF (HeadersFrame_ACF f))   = do 
    new_frame <- justCompress f
    return $ wrapCF new_frame 
compressFrameHeaders frame_without_headers                   = 
    return frame_without_headers


uncompressFrameHeaders :: AnyFrame -> (SessionM IO) AnyFrame 
uncompressFrameHeaders ( AnyControl_AF (SynStream_ACF f))      = do 
    new_frame <- justDecompress f
    return $ wrapCF new_frame 
uncompressFrameHeaders ( AnyControl_AF (SynReplyFrame_ACF f )) = do 
    new_frame <- justDecompress f
    return $ wrapCF new_frame 
uncompressFrameHeaders ( AnyControl_AF (HeadersFrame_ACF f))   = do 
    new_frame <- justDecompress f
    return $ wrapCF new_frame 
uncompressFrameHeaders frame_without_headers                   = 
    return frame_without_headers


justCompress :: CompressedHeadersOnFrame f => f -> SessionM IO f
justCompress frame = do 
    send_zlib_mvar <-  asks sendZLib
    case present_headers of 

        UncompressedKeyValueBlock uncompressed_uvl -> do
            
            uncompressed_bytes <- return $ LB.toStrict $ runPut $ Bi.put $ uncompressed_uvl
            
            new_value <- liftIO $ do
                withMVar send_zlib_mvar $ \ send_zlib -> do
                    popper       <-  Z.feedDeflate send_zlib uncompressed_bytes
                    list_piece_1 <-  exhaustPopper popper 
                    latest_piece <-  exhaustPopper $ Z.flushDeflate send_zlib
                    return $ CompressedKeyValueBlock $ B.concat (list_piece_1 ++ latest_piece)

            return $ setCompressedHeaders frame new_value

        CompressedKeyValueBlock _     -> error "This was not expected"
  where 
    present_headers = getCompressedHeaders frame


justDecompress :: CompressedHeadersOnFrame f => f -> SessionM IO f
justDecompress frame = do 
    recv_zlib_mvar <-  asks recvZLib
    case present_headers of 

        CompressedKeyValueBlock bscmp -> do
            
            -- uncompressed_bytes <- return $ LB.toStrict $ runPut $ Bi.put $ uncompressed_uvl
            
            new_value <- liftIO $ do
                withMVar recv_zlib_mvar $ \ recv_zlib -> do
                    popper             <- Z.feedInflate recv_zlib bscmp
                    list_piece_1       <- exhaustPopper popper 
                    latest             <- Z.flushInflate recv_zlib
                    uncompressed_bytes <- return $ B.concat (list_piece_1 ++ [latest])  
                    return $ UncompressedKeyValueBlock $ runGet Bi.get $ LB.fromChunks [uncompressed_bytes]

            return $ setCompressedHeaders frame new_value

        UncompressedKeyValueBlock _     -> error "This was not expected"
  where 
    present_headers = getCompressedHeaders frame


exhaustPopper :: Z.Popper   -> IO [B.ByteString]
exhaustPopper popper = do 
    x                  <- popper 
    case x of 
        Z.PRDone            -> return []

        Z.PRNext bytestring -> do 
            more <- exhaustPopper popper 
            return $ (bytestring:more)

        Z.PRError e         -> do 
            -- When this happens, the only sensible 
            -- thing to do is throw an exception, and trash the entire 
            -- stream.... 
            throwIO  e


zLibInitDict :: B.ByteString
zLibInitDict = pack $ map toEnum  [
    0x00, 0x00, 0x00, 0x07, 0x6f, 0x70, 0x74, 0x69,    
    0x6f, 0x6e, 0x73, 0x00, 0x00, 0x00, 0x04, 0x68,    
    0x65, 0x61, 0x64, 0x00, 0x00, 0x00, 0x04, 0x70,    
    0x6f, 0x73, 0x74, 0x00, 0x00, 0x00, 0x03, 0x70,    
    0x75, 0x74, 0x00, 0x00, 0x00, 0x06, 0x64, 0x65,    
    0x6c, 0x65, 0x74, 0x65, 0x00, 0x00, 0x00, 0x05,    
    0x74, 0x72, 0x61, 0x63, 0x65, 0x00, 0x00, 0x00,    
    0x06, 0x61, 0x63, 0x63, 0x65, 0x70, 0x74, 0x00,    
    0x00, 0x00, 0x0e, 0x61, 0x63, 0x63, 0x65, 0x70,    
    0x74, 0x2d, 0x63, 0x68, 0x61, 0x72, 0x73, 0x65,    
    0x74, 0x00, 0x00, 0x00, 0x0f, 0x61, 0x63, 0x63,    
    0x65, 0x70, 0x74, 0x2d, 0x65, 0x6e, 0x63, 0x6f,    
    0x64, 0x69, 0x6e, 0x67, 0x00, 0x00, 0x00, 0x0f,    
    0x61, 0x63, 0x63, 0x65, 0x70, 0x74, 0x2d, 0x6c,    
    0x61, 0x6e, 0x67, 0x75, 0x61, 0x67, 0x65, 0x00,    
    0x00, 0x00, 0x0d, 0x61, 0x63, 0x63, 0x65, 0x70,    
    0x74, 0x2d, 0x72, 0x61, 0x6e, 0x67, 0x65, 0x73,    
    0x00, 0x00, 0x00, 0x03, 0x61, 0x67, 0x65, 0x00,    
    0x00, 0x00, 0x05, 0x61, 0x6c, 0x6c, 0x6f, 0x77,    
    0x00, 0x00, 0x00, 0x0d, 0x61, 0x75, 0x74, 0x68,    
    0x6f, 0x72, 0x69, 0x7a, 0x61, 0x74, 0x69, 0x6f,    
    0x6e, 0x00, 0x00, 0x00, 0x0d, 0x63, 0x61, 0x63,    
    0x68, 0x65, 0x2d, 0x63, 0x6f, 0x6e, 0x74, 0x72,    
    0x6f, 0x6c, 0x00, 0x00, 0x00, 0x0a, 0x63, 0x6f,    
    0x6e, 0x6e, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e,    
    0x00, 0x00, 0x00, 0x0c, 0x63, 0x6f, 0x6e, 0x74,    
    0x65, 0x6e, 0x74, 0x2d, 0x62, 0x61, 0x73, 0x65,    
    0x00, 0x00, 0x00, 0x10, 0x63, 0x6f, 0x6e, 0x74,    
    0x65, 0x6e, 0x74, 0x2d, 0x65, 0x6e, 0x63, 0x6f,    
    0x64, 0x69, 0x6e, 0x67, 0x00, 0x00, 0x00, 0x10,    
    0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x2d,    
    0x6c, 0x61, 0x6e, 0x67, 0x75, 0x61, 0x67, 0x65,    
    0x00, 0x00, 0x00, 0x0e, 0x63, 0x6f, 0x6e, 0x74,    
    0x65, 0x6e, 0x74, 0x2d, 0x6c, 0x65, 0x6e, 0x67,    
    0x74, 0x68, 0x00, 0x00, 0x00, 0x10, 0x63, 0x6f,    
    0x6e, 0x74, 0x65, 0x6e, 0x74, 0x2d, 0x6c, 0x6f,    
    0x63, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x00, 0x00,    
    0x00, 0x0b, 0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e,    
    0x74, 0x2d, 0x6d, 0x64, 0x35, 0x00, 0x00, 0x00,    
    0x0d, 0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74,    
    0x2d, 0x72, 0x61, 0x6e, 0x67, 0x65, 0x00, 0x00,    
    0x00, 0x0c, 0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e,    
    0x74, 0x2d, 0x74, 0x79, 0x70, 0x65, 0x00, 0x00,    
    0x00, 0x04, 0x64, 0x61, 0x74, 0x65, 0x00, 0x00,    
    0x00, 0x04, 0x65, 0x74, 0x61, 0x67, 0x00, 0x00,    
    0x00, 0x06, 0x65, 0x78, 0x70, 0x65, 0x63, 0x74,    
    0x00, 0x00, 0x00, 0x07, 0x65, 0x78, 0x70, 0x69,    
    0x72, 0x65, 0x73, 0x00, 0x00, 0x00, 0x04, 0x66,    
    0x72, 0x6f, 0x6d, 0x00, 0x00, 0x00, 0x04, 0x68,    
    0x6f, 0x73, 0x74, 0x00, 0x00, 0x00, 0x08, 0x69,    
    0x66, 0x2d, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x00,    
    0x00, 0x00, 0x11, 0x69, 0x66, 0x2d, 0x6d, 0x6f,    
    0x64, 0x69, 0x66, 0x69, 0x65, 0x64, 0x2d, 0x73,    
    0x69, 0x6e, 0x63, 0x65, 0x00, 0x00, 0x00, 0x0d,    
    0x69, 0x66, 0x2d, 0x6e, 0x6f, 0x6e, 0x65, 0x2d,    
    0x6d, 0x61, 0x74, 0x63, 0x68, 0x00, 0x00, 0x00,    
    0x08, 0x69, 0x66, 0x2d, 0x72, 0x61, 0x6e, 0x67,    
    0x65, 0x00, 0x00, 0x00, 0x13, 0x69, 0x66, 0x2d,    
    0x75, 0x6e, 0x6d, 0x6f, 0x64, 0x69, 0x66, 0x69,    
    0x65, 0x64, 0x2d, 0x73, 0x69, 0x6e, 0x63, 0x65,    
    0x00, 0x00, 0x00, 0x0d, 0x6c, 0x61, 0x73, 0x74,    
    0x2d, 0x6d, 0x6f, 0x64, 0x69, 0x66, 0x69, 0x65,    
    0x64, 0x00, 0x00, 0x00, 0x08, 0x6c, 0x6f, 0x63,    
    0x61, 0x74, 0x69, 0x6f, 0x6e, 0x00, 0x00, 0x00,    
    0x0c, 0x6d, 0x61, 0x78, 0x2d, 0x66, 0x6f, 0x72,    
    0x77, 0x61, 0x72, 0x64, 0x73, 0x00, 0x00, 0x00,    
    0x06, 0x70, 0x72, 0x61, 0x67, 0x6d, 0x61, 0x00,    
    0x00, 0x00, 0x12, 0x70, 0x72, 0x6f, 0x78, 0x79,    
    0x2d, 0x61, 0x75, 0x74, 0x68, 0x65, 0x6e, 0x74,    
    0x69, 0x63, 0x61, 0x74, 0x65, 0x00, 0x00, 0x00,    
    0x13, 0x70, 0x72, 0x6f, 0x78, 0x79, 0x2d, 0x61,    
    0x75, 0x74, 0x68, 0x6f, 0x72, 0x69, 0x7a, 0x61,    
    0x74, 0x69, 0x6f, 0x6e, 0x00, 0x00, 0x00, 0x05,    
    0x72, 0x61, 0x6e, 0x67, 0x65, 0x00, 0x00, 0x00,    
    0x07, 0x72, 0x65, 0x66, 0x65, 0x72, 0x65, 0x72,    
    0x00, 0x00, 0x00, 0x0b, 0x72, 0x65, 0x74, 0x72,    
    0x79, 0x2d, 0x61, 0x66, 0x74, 0x65, 0x72, 0x00,    
    0x00, 0x00, 0x06, 0x73, 0x65, 0x72, 0x76, 0x65,    
    0x72, 0x00, 0x00, 0x00, 0x02, 0x74, 0x65, 0x00,    
    0x00, 0x00, 0x07, 0x74, 0x72, 0x61, 0x69, 0x6c,    
    0x65, 0x72, 0x00, 0x00, 0x00, 0x11, 0x74, 0x72,    
    0x61, 0x6e, 0x73, 0x66, 0x65, 0x72, 0x2d, 0x65,    
    0x6e, 0x63, 0x6f, 0x64, 0x69, 0x6e, 0x67, 0x00,    
    0x00, 0x00, 0x07, 0x75, 0x70, 0x67, 0x72, 0x61,    
    0x64, 0x65, 0x00, 0x00, 0x00, 0x0a, 0x75, 0x73,    
    0x65, 0x72, 0x2d, 0x61, 0x67, 0x65, 0x6e, 0x74,    
    0x00, 0x00, 0x00, 0x04, 0x76, 0x61, 0x72, 0x79,    
    0x00, 0x00, 0x00, 0x03, 0x76, 0x69, 0x61, 0x00,    
    0x00, 0x00, 0x07, 0x77, 0x61, 0x72, 0x6e, 0x69,    
    0x6e, 0x67, 0x00, 0x00, 0x00, 0x10, 0x77, 0x77,    
    0x77, 0x2d, 0x61, 0x75, 0x74, 0x68, 0x65, 0x6e,    
    0x74, 0x69, 0x63, 0x61, 0x74, 0x65, 0x00, 0x00,    
    0x00, 0x06, 0x6d, 0x65, 0x74, 0x68, 0x6f, 0x64,    
    0x00, 0x00, 0x00, 0x03, 0x67, 0x65, 0x74, 0x00,    
    0x00, 0x00, 0x06, 0x73, 0x74, 0x61, 0x74, 0x75,    
    0x73, 0x00, 0x00, 0x00, 0x06, 0x32, 0x30, 0x30,    
    0x20, 0x4f, 0x4b, 0x00, 0x00, 0x00, 0x07, 0x76,    
    0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x00, 0x00,    
    0x00, 0x08, 0x48, 0x54, 0x54, 0x50, 0x2f, 0x31,    
    0x2e, 0x31, 0x00, 0x00, 0x00, 0x03, 0x75, 0x72,    
    0x6c, 0x00, 0x00, 0x00, 0x06, 0x70, 0x75, 0x62,    
    0x6c, 0x69, 0x63, 0x00, 0x00, 0x00, 0x0a, 0x73,    
    0x65, 0x74, 0x2d, 0x63, 0x6f, 0x6f, 0x6b, 0x69,    
    0x65, 0x00, 0x00, 0x00, 0x0a, 0x6b, 0x65, 0x65,    
    0x70, 0x2d, 0x61, 0x6c, 0x69, 0x76, 0x65, 0x00,    
    0x00, 0x00, 0x06, 0x6f, 0x72, 0x69, 0x67, 0x69,    
    0x6e, 0x31, 0x30, 0x30, 0x31, 0x30, 0x31, 0x32,    
    0x30, 0x31, 0x32, 0x30, 0x32, 0x32, 0x30, 0x35,    
    0x32, 0x30, 0x36, 0x33, 0x30, 0x30, 0x33, 0x30,    
    0x32, 0x33, 0x30, 0x33, 0x33, 0x30, 0x34, 0x33,    
    0x30, 0x35, 0x33, 0x30, 0x36, 0x33, 0x30, 0x37,    
    0x34, 0x30, 0x32, 0x34, 0x30, 0x35, 0x34, 0x30,    
    0x36, 0x34, 0x30, 0x37, 0x34, 0x30, 0x38, 0x34,    
    0x30, 0x39, 0x34, 0x31, 0x30, 0x34, 0x31, 0x31,    
    0x34, 0x31, 0x32, 0x34, 0x31, 0x33, 0x34, 0x31,    
    0x34, 0x34, 0x31, 0x35, 0x34, 0x31, 0x36, 0x34,    
    0x31, 0x37, 0x35, 0x30, 0x32, 0x35, 0x30, 0x34,    
    0x35, 0x30, 0x35, 0x32, 0x30, 0x33, 0x20, 0x4e,    
    0x6f, 0x6e, 0x2d, 0x41, 0x75, 0x74, 0x68, 0x6f,    
    0x72, 0x69, 0x74, 0x61, 0x74, 0x69, 0x76, 0x65,    
    0x20, 0x49, 0x6e, 0x66, 0x6f, 0x72, 0x6d, 0x61,    
    0x74, 0x69, 0x6f, 0x6e, 0x32, 0x30, 0x34, 0x20,    
    0x4e, 0x6f, 0x20, 0x43, 0x6f, 0x6e, 0x74, 0x65,    
    0x6e, 0x74, 0x33, 0x30, 0x31, 0x20, 0x4d, 0x6f,    
    0x76, 0x65, 0x64, 0x20, 0x50, 0x65, 0x72, 0x6d,    
    0x61, 0x6e, 0x65, 0x6e, 0x74, 0x6c, 0x79, 0x34,    
    0x30, 0x30, 0x20, 0x42, 0x61, 0x64, 0x20, 0x52,    
    0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x34, 0x30,    
    0x31, 0x20, 0x55, 0x6e, 0x61, 0x75, 0x74, 0x68,    
    0x6f, 0x72, 0x69, 0x7a, 0x65, 0x64, 0x34, 0x30,    
    0x33, 0x20, 0x46, 0x6f, 0x72, 0x62, 0x69, 0x64,    
    0x64, 0x65, 0x6e, 0x34, 0x30, 0x34, 0x20, 0x4e,    
    0x6f, 0x74, 0x20, 0x46, 0x6f, 0x75, 0x6e, 0x64,    
    0x35, 0x30, 0x30, 0x20, 0x49, 0x6e, 0x74, 0x65,    
    0x72, 0x6e, 0x61, 0x6c, 0x20, 0x53, 0x65, 0x72,    
    0x76, 0x65, 0x72, 0x20, 0x45, 0x72, 0x72, 0x6f,    
    0x72, 0x35, 0x30, 0x31, 0x20, 0x4e, 0x6f, 0x74,    
    0x20, 0x49, 0x6d, 0x70, 0x6c, 0x65, 0x6d, 0x65,    
    0x6e, 0x74, 0x65, 0x64, 0x35, 0x30, 0x33, 0x20,    
    0x53, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x20,    
    0x55, 0x6e, 0x61, 0x76, 0x61, 0x69, 0x6c, 0x61,    
    0x62, 0x6c, 0x65, 0x4a, 0x61, 0x6e, 0x20, 0x46,    
    0x65, 0x62, 0x20, 0x4d, 0x61, 0x72, 0x20, 0x41,    
    0x70, 0x72, 0x20, 0x4d, 0x61, 0x79, 0x20, 0x4a,    
    0x75, 0x6e, 0x20, 0x4a, 0x75, 0x6c, 0x20, 0x41,    
    0x75, 0x67, 0x20, 0x53, 0x65, 0x70, 0x74, 0x20,    
    0x4f, 0x63, 0x74, 0x20, 0x4e, 0x6f, 0x76, 0x20,    
    0x44, 0x65, 0x63, 0x20, 0x30, 0x30, 0x3a, 0x30,    
    0x30, 0x3a, 0x30, 0x30, 0x20, 0x4d, 0x6f, 0x6e,    
    0x2c, 0x20, 0x54, 0x75, 0x65, 0x2c, 0x20, 0x57,    
    0x65, 0x64, 0x2c, 0x20, 0x54, 0x68, 0x75, 0x2c,    
    0x20, 0x46, 0x72, 0x69, 0x2c, 0x20, 0x53, 0x61,    
    0x74, 0x2c, 0x20, 0x53, 0x75, 0x6e, 0x2c, 0x20,    
    0x47, 0x4d, 0x54, 0x63, 0x68, 0x75, 0x6e, 0x6b,    
    0x65, 0x64, 0x2c, 0x74, 0x65, 0x78, 0x74, 0x2f,    
    0x68, 0x74, 0x6d, 0x6c, 0x2c, 0x69, 0x6d, 0x61,    
    0x67, 0x65, 0x2f, 0x70, 0x6e, 0x67, 0x2c, 0x69,    
    0x6d, 0x61, 0x67, 0x65, 0x2f, 0x6a, 0x70, 0x67,    
    0x2c, 0x69, 0x6d, 0x61, 0x67, 0x65, 0x2f, 0x67,    
    0x69, 0x66, 0x2c, 0x61, 0x70, 0x70, 0x6c, 0x69,    
    0x63, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x2f, 0x78,    
    0x6d, 0x6c, 0x2c, 0x61, 0x70, 0x70, 0x6c, 0x69,    
    0x63, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x2f, 0x78,    
    0x68, 0x74, 0x6d, 0x6c, 0x2b, 0x78, 0x6d, 0x6c,    
    0x2c, 0x74, 0x65, 0x78, 0x74, 0x2f, 0x70, 0x6c,    
    0x61, 0x69, 0x6e, 0x2c, 0x74, 0x65, 0x78, 0x74,    
    0x2f, 0x6a, 0x61, 0x76, 0x61, 0x73, 0x63, 0x72,    
    0x69, 0x70, 0x74, 0x2c, 0x70, 0x75, 0x62, 0x6c,    
    0x69, 0x63, 0x70, 0x72, 0x69, 0x76, 0x61, 0x74,    
    0x65, 0x6d, 0x61, 0x78, 0x2d, 0x61, 0x67, 0x65,    
    0x3d, 0x67, 0x7a, 0x69, 0x70, 0x2c, 0x64, 0x65,    
    0x66, 0x6c, 0x61, 0x74, 0x65, 0x2c, 0x73, 0x64,    
    0x63, 0x68, 0x63, 0x68, 0x61, 0x72, 0x73, 0x65,    
    0x74, 0x3d, 0x75, 0x74, 0x66, 0x2d, 0x38, 0x63,    
    0x68, 0x61, 0x72, 0x73, 0x65, 0x74, 0x3d, 0x69,    
    0x73, 0x6f, 0x2d, 0x38, 0x38, 0x35, 0x39, 0x2d,    
    0x31, 0x2c, 0x75, 0x74, 0x66, 0x2d, 0x2c, 0x2a,    
    0x2c, 0x65, 0x6e, 0x71, 0x3d, 0x30, 0x2e           
    ]

