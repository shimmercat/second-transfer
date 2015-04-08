-- The framer has two functions: to convert bytes to Frames and the other way around,
-- and two keep track of flow-control quotas. 
{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances, 
             DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http2.Framer (
    BadPrefaceException,

    wrapSession,
    http2FrameLength,

    -- Not needed anywhere, but supress the warning about unneeded symbol
    closeAction
    ) where 



import           Control.Concurrent
import           Control.Exception
import qualified Control.Exception                      as E
import           Control.Lens                           (view)
import qualified Control.Lens                           as L
import           Control.Lens                           ((^.))
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Reader
import           Data.Binary                            (decode)
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Lazy                   as LB
import           Data.Conduit
import           Data.Foldable                          (find)
import           Data.Typeable                          (Typeable)

import qualified Network.HTTP2                          as NH2
-- Logging utilities
import           System.Log.Logger

import qualified Data.HashTable.IO                      as H

import           SecondTransfer.Http2.Session           
import           SecondTransfer.MainLoop.CoherentWorker (CoherentWorker)
import qualified SecondTransfer.MainLoop.Framer         as F
import           SecondTransfer.MainLoop.PushPullType   (Attendant, CloseAction,
                                                         PullAction, PushAction)
import           SecondTransfer.Utils                   (Word24, word24ToInt)
import           SecondTransfer.Exception


http2PrefixLength :: Int
http2PrefixLength = B.length NH2.connectionPreface



-- Let's do flow control here here .... 

type HashTable k v = H.CuckooHashTable k v


type GlobalStreamId = Int


data FlowControlCommand = 
     AddBytes_FCM Int 

-- A hashtable from stream id to channel of availabiliy increases
type Stream2AvailSpace = HashTable GlobalStreamId (Chan FlowControlCommand)


data CanOutput = CanOutput


data NoHeadersInChannel = NoHeadersInChannel


data FramerSessionData = FramerSessionData {
      _stream2flow           :: Stream2AvailSpace
    , _stream2outputBytes    :: HashTable GlobalStreamId (Chan LB.ByteString)
    , _defaultStreamWindow   :: MVar Int

    , _canOutput             :: MVar CanOutput
    , _noHeadersInChannel    :: MVar NoHeadersInChannel
    , _pushAction            :: PushAction
    , _closeAction           :: CloseAction

    -- Global id of the session, used for e.g. error reporting.
    , _sessionId             :: Int 

    -- Sessions context, used for thing like e.g. error reporting
    , _sessionsContext       :: SessionsContext
    }


L.makeLenses ''FramerSessionData


type FramerSession = ReaderT FramerSessionData IO


wrapSession :: CoherentWorker -> SessionsContext -> Attendant
wrapSession coherent_worker sessions_context push_action pull_action close_action = do

    let 
        session_id_mvar = view nextSessionId sessions_context

    new_session_id <- modifyMVarMasked
        session_id_mvar
        (\ session_id -> return (session_id+1, session_id))

    (session_input, session_output) <- (http2Session 
        coherent_worker new_session_id sessions_context)

    -- TODO : Add type annotations....
    s2f <- H.new 
    s2o <- H.new 
    default_stream_size_mvar <- newMVar 65536
    can_output <- newMVar CanOutput
    no_headers_in_channel <- newMVar NoHeadersInChannel


    -- We need some shared state 
    let framer_session_data = FramerSessionData {
        _stream2flow          = s2f
        ,_stream2outputBytes  = s2o 
        ,_defaultStreamWindow = default_stream_size_mvar
        ,_canOutput           = can_output 
        ,_noHeadersInChannel  = no_headers_in_channel
        ,_pushAction          = push_action
        ,_closeAction         = close_action
        ,_sessionId           = new_session_id
        ,_sessionsContext     = sessions_context
        }

    forkIO 
        $ close_on_error new_session_id sessions_context 
        $ runReaderT (inputGatherer pull_action session_input ) framer_session_data  
    forkIO 
        $ close_on_error new_session_id sessions_context 
        $ runReaderT (outputGatherer session_output ) framer_session_data 

    return ()

  where 
    close_on_error session_id session_context comp = E.finally (
        E.catch comp (exc_handler session_id session_context)) close_action

    exc_handler :: Int -> SessionsContext -> FramerException -> IO ()
    exc_handler x y e = do
        sessionExceptionHandler Framer_SessionComponent x y e
        throwIO e


http2FrameLength :: F.LengthCallback
http2FrameLength bs | (B.length bs) >= 3     = let
    word24 = decode input_as_lbs :: Word24
    input_as_lbs = LB.fromStrict bs
  in 
    Just $ (word24ToInt word24) + 9 -- Nine bytes that the frame header always uses
http2FrameLength _ = Nothing


addCapacity :: 
        GlobalStreamId ->
        Int           -> 
        FramerSession ()
addCapacity stream_id delta_cap = do 

    if stream_id == 0 
      then 
        -- TODO: Add session flow control
        return ()
      else do
        table <- view stream2flow  
        val <- liftIO $ H.lookup table stream_id
        case val of 
            Nothing -> do
                -- ??
                liftIO $ putStrLn $ "Tried to update window of unexistent stream (creating): " ++ (show stream_id)
                (_, command_chan) <- startStreamOutputQueue stream_id 
                -- And try again
                liftIO $ writeChan command_chan $ AddBytes_FCM delta_cap


            Just command_chan -> do
                liftIO $ writeChan command_chan $ AddBytes_FCM delta_cap
        

-- This works by pulling bytes from the input side of the pipeline and converting them to frames.
-- The frames are then put in the SessionInput. In the other end of the SessionInput they can be 
-- interpreted according to their HTTP/2 meaning. 
-- 
-- This function also does part of the flow control: it registers WindowUpdate frames and triggers
-- quota updates on the streams. 
inputGatherer :: PullAction -> SessionInput -> FramerSession ()
inputGatherer pull_action session_input = do 
    -- We can start by reading off the prefix....
    (prefix, remaining) <- liftIO $ F.readLength http2PrefixLength pull_action

    if prefix /= NH2.connectionPreface 
      then 
        liftIO $ do 
            throwIO BadPrefaceException
      else 
        return ()
    let 
        source::Source FramerSession B.ByteString
        source = transPipe liftIO $ F.readNextChunk http2FrameLength remaining pull_action
    ( source $$ consume)
  where 

    consume :: Sink B.ByteString FramerSession ()
    consume = do 
        maybe_bytes <- await 
        -- Deserialize

        case maybe_bytes of 

            Just bytes -> do
                let 
                    error_or_frame = NH2.decodeFrame some_settings bytes
                    -- TODO: See how we can change these....
                    some_settings = NH2.defaultSettings

                case error_or_frame of 

                    Left some_error -> do 
                        liftIO $ putStrLn $ "Got an error: " ++ (show some_error)


                    Right (NH2.Frame (NH2.FrameHeader _ _ stream_id) (NH2.WindowUpdateFrame credit) ) -> do 
                        -- Bookkeep the increase on bytes on that stream
                        -- liftIO $ putStrLn $ "Extra capacity for stream " ++ (show stream_id)
                        lift $ addCapacity (NH2.fromStreamIdentifier stream_id) (fromIntegral credit)
                        return ()


                    Right frame@(NH2.Frame _ (NH2.SettingsFrame settings_list) ) -> do 
                        -- Increase all the stuff....
                        case find (\(i,_) -> i == NH2.SettingsInitialWindowSize) settings_list of 

                            Just (_, new_default_stream_size) -> do 
                                old_default_stream_size_mvar <- view defaultStreamWindow
                                old_default_stream_size <- liftIO $ takeMVar old_default_stream_size_mvar
                                let general_delta = new_default_stream_size - old_default_stream_size
                                stream_to_flow <- view stream2flow
                                -- Add capacity to everybody's windows
                                liftIO $ 
                                    H.mapM_ (
                                            \ (k,v) -> if k /=0 
                                                          then writeChan v (AddBytes_FCM general_delta) 
                                                          else return () )
                                            stream_to_flow


                                -- And set a new value 
                                liftIO $ putMVar old_default_stream_size_mvar new_default_stream_size


                            Nothing -> 
                                return ()

                        -- And send the frame down to the session
                        liftIO $ sendFrameToSession session_input frame


                    Right a_frame   -> do 
                        liftIO $ sendFrameToSession session_input a_frame

                -- tail recursion: go again...
                consume 

            Nothing    -> 
                -- We may as well exit this thread
               return ()


outputGatherer :: SessionOutput -> FramerSession ()
outputGatherer session_output = do 

    -- We start by sending a settings frame 
    pushFrame 
        (NH2.EncodeInfo NH2.defaultFlags (NH2.toStreamIdentifier 0) Nothing)
        (NH2.SettingsFrame [])

    loopPart

  where 


    dataForFrame p1 p2 = 
        LB.fromStrict $ NH2.encodeFrame p1 p2

    loopPart :: FramerSession ()
    loopPart = do 

        command_or_frame  <- liftIO $ getFrameFromSession session_output

        case command_or_frame of 

            Left cmd -> do 
                -- TODO: This is just a quickie behavior I dropped 
                -- here, semantics need to be different probably.
                liftIO $ putStrLn $ "Received a command... terminating " ++ (show cmd)


            Right ( p1@(NH2.EncodeInfo _ stream_idii _), p2@(NH2.DataFrame _) ) -> do
                -- This frame is flow-controlled... I may be unable to send this frame in
                -- some circumstances... 
                let stream_id = NH2.fromStreamIdentifier stream_idii
                s2o <- view stream2outputBytes
                lookup_result <- liftIO $ H.lookup s2o stream_id 
                stream_bytes_chan <- case lookup_result of 

                    Nothing ->  do 
                        (bc, _) <- startStreamOutputQueue stream_id
                        return bc

                    Just bytes_chan -> return bytes_chan 

                liftIO $ writeChan stream_bytes_chan $ dataForFrame p1 p2

                loopPart


            Right (p1, p2@(NH2.HeadersFrame _ _) ) -> do
                handleHeadersOfStream p1 p2
                
                loopPart


            Right (p1, p2@(NH2.ContinuationFrame _) ) -> do
                handleHeadersOfStream p1 p2

                loopPart

            Right (p1, p2) -> do 
                -- Most other frames go right away... as long as no headers are in process...
                no_headers <- view noHeadersInChannel
                liftIO $ takeMVar no_headers
                pushFrame p1 p2 
                liftIO $ putMVar no_headers NoHeadersInChannel
                
                loopPart


startStreamOutputQueue :: Int -> FramerSession (Chan LB.ByteString, Chan FlowControlCommand)
startStreamOutputQueue stream_id = do
    -- New thread for handling outputs of this stream is needed
    bytes_chan <- liftIO newChan 
    command_chan <- liftIO newChan 
    s2o <- view stream2outputBytes
    liftIO $ H.insert s2o stream_id bytes_chan 
    s2c <- view stream2flow
    liftIO $ H.insert s2c stream_id command_chan 
    initial_cap_mvar <- view defaultStreamWindow
    initial_cap <- liftIO $ readMVar initial_cap_mvar

    -- And don't forget the thread itself
    read_state <- ask 
    liftIO $ forkIO $ runReaderT
        (flowControlOutput stream_id initial_cap "" command_chan bytes_chan)
        read_state

    return (bytes_chan , command_chan)


-- This works in the output side of the HTTP/2 framing session, and it acts as a 
-- semaphore ensuring that headers are output without any interleaved frames. 
handleHeadersOfStream :: NH2.EncodeInfo -> NH2.FramePayload -> FramerSession ()
handleHeadersOfStream p1@(NH2.EncodeInfo _ _ _) frame_payload
    | (frameIsHeaderOfStream frame_payload) && (not $ frameEndsHeaders p1 frame_payload) = do
        -- Take it 
        no_headers <- view noHeadersInChannel
        liftIO $ takeMVar no_headers
        pushFrame p1 frame_payload 
        -- DONT PUT THE MvAR HERE 

    | (frameIsHeaderOfStream frame_payload) && (frameEndsHeaders p1 frame_payload) = do
        no_headers <- view noHeadersInChannel
        liftIO $ takeMVar no_headers
        pushFrame p1 frame_payload 
        -- Since we finish.... 
        liftIO $ putMVar no_headers NoHeadersInChannel

    | frameEndsHeaders p1 frame_payload = do 
        -- I can only get here for a continuation frame  after something else that is a headers
        no_headers <- view noHeadersInChannel
        result <- liftIO $ tryPutMVar no_headers NoHeadersInChannel
        liftIO $ putStrLn $ "Could put MVAR (yes must be): " ++ (show result)


frameIsHeaderOfStream :: NH2.FramePayload -> Bool
frameIsHeaderOfStream (NH2.HeadersFrame _  _ )
    = True
frameIsHeaderOfStream _                                       
    = False 


frameEndsHeaders  :: NH2.EncodeInfo -> NH2.FramePayload -> Bool 
frameEndsHeaders (NH2.EncodeInfo flags _ _) (NH2.HeadersFrame _ _) = NH2.testEndHeader flags
frameEndsHeaders (NH2.EncodeInfo flags _ _) (NH2.ContinuationFrame _) = NH2.testEndHeader flags
frameEndsHeaders _ _ = False


pushFrame :: NH2.EncodeInfo
             -> NH2.FramePayload -> FramerSession ()
pushFrame p1 p2 = do
    let bs = LB.fromStrict $ NH2.encodeFrame p1 p2  
    sendBytes bs


sendBytes :: LB.ByteString -> FramerSession ()
sendBytes bs = do
    push_action <- view pushAction
    can_output <- view canOutput 
    liftIO $ do 
        bs `seq` takeMVar can_output
        push_action bs
        putMVar  can_output CanOutput


-- A thread in charge of doing flow control transmission
-- TODO: Do session flow control..... 
flowControlOutput :: Int -> Int -> LB.ByteString -> (Chan FlowControlCommand) -> (Chan LB.ByteString) ->  FramerSession ()
flowControlOutput stream_id capacity leftovers commands_chan bytes_chan = do 
    -- Get some bytes to send 
    

    if leftovers == "" 
      then do
        -- Get more data (possibly block waiting for it)
        bytes_to_send <- liftIO $ readChan bytes_chan
        flowControlOutput stream_id capacity  bytes_to_send commands_chan bytes_chan
      else do
        -- Length?
        let amount = fromIntegral $ ((LB.length leftovers) - 9)
        if  amount <= capacity 
          then do
            -- I can send ... if no headers are in process....
            no_headers <- view noHeadersInChannel
            liftIO $ takeMVar no_headers
            sendBytes leftovers
            -- liftIO $ putStrLn $ "Sent flow-controlled data for " ++ (show stream_id)
            -- liftIO $ putStrLn $ "Capacity left " ++ (show (capacity - amount))
            liftIO $ putMVar no_headers NoHeadersInChannel
            -- and tail-invoke 
            flowControlOutput  stream_id (capacity - amount) "" commands_chan bytes_chan
          else do
            -- I can not send because flow-control is full, wait for a command instead 
            liftIO $ putStrLn $ "Warning: channel flow-saturated " ++ (show stream_id)
            command <- liftIO $ readChan commands_chan
            case command of 
                AddBytes_FCM delta_cap -> do 
                    -- liftIO $ putStrLn $ "Flow control delta_cap stream " ++ (show stream_id)
                    flowControlOutput stream_id (capacity + delta_cap) leftovers commands_chan bytes_chan