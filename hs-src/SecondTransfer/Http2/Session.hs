-- Session: links frames to streams, and helps in ordering the header frames
-- so that they don't get mixed with header frames from other streams when
-- resources are being served concurrently.
{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http2.Session(
    http2ServerSession
    ,http2ClientSession
    ,getFrameFromSession
    ,sendFirstFrameToSession
    ,sendMiddleFrameToSession
    ,sendCommandToSession
    ,makeClientState
    ,pendingRequests_ClS

    ,CoherentSession
    ,SessionInput(..)
    ,SessionInputCommand(..)
    ,SessionOutput(..)
    ,SessionCoordinates(..)
    ,SessionComponent(..)
    ,SessionsCallbacks(..)
    ,SessionsConfig(..)
    ,ErrorCallback
    ,ClientState(..)
    ,SessionRole(..)

    -- Internal stuff
    ,InputFrame
    ,nextPushStream  -- Exporting just to hide the warning
    ) where


-- System grade utilities imports
import           Control.Concurrent                     (ThreadId)
import           Control.Concurrent.Chan
import           Control.Exception                      (throwTo)
import qualified Control.Exception                      as E
import           Control.Monad                          (
                                                         forever,
                                                         unless,
                                                         when,
--                                                         mapM_,
                                                         forM,
                                                         forM_)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.DeepSeq                        (
                                                         --($!!),
                                                        deepseq )
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Catch                    as CMC

import           Control.Concurrent.MVar
import qualified Data.ByteString                        as B
--import           Data.ByteString.Char8                  (pack,unpack)
import qualified Data.ByteString.Builder                as Bu
import qualified Data.ByteString.Lazy                   as Bl
import           Data.Conduit
--import qualified Data.Conduit                           as Cnd
import qualified Data.HashTable.IO                      as H
import qualified Data.IntSet                            as NS
import           Data.Maybe                             (isJust)
import qualified Data.IORef                             as DIO
import           Data.Typeable

import           Control.Lens

-- No framing layer here... let's use Kazu's Yamamoto library
import qualified Network.HPACK                          as HP
import qualified Network.HTTP2                          as NH2


import           System.Clock                            ( getTime
                                                         , Clock(..))

-- Imports from other parts of the program
import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.MainLoop.Tokens
import           SecondTransfer.MainLoop.Protocol
import           SecondTransfer.MainLoop.ClientPetitioner

import           SecondTransfer.Sessions.Config
import           SecondTransfer.Sessions.Internal       (sessionExceptionHandler,
                                                         SessionsContext,
                                                         sessionsConfig)
import           SecondTransfer.Utils                   (unfoldChannelAndSource)
import           SecondTransfer.Exception
import qualified SecondTransfer.Utils.HTTPHeaders       as He
import qualified SecondTransfer.Http2.TransferTypes     as TT
#ifdef SECONDTRANSFER_MONITORING
import           SecondTransfer.MainLoop.Logging        (logit)
#endif

--import           Debug.Trace


type InputFrame  = NH2.Frame


--useChunkLength :: Int
-- useChunkLength = 2048


-- Singleton instance used for concurrency
data HeadersSent = HeadersSent

-- All streams put their data bits here. A "Nothing" value signals
-- end of data. Middle value is delay in microseconds
type DataOutputToConveyor = (GlobalStreamId, Maybe B.ByteString, Effect)

-- What to do regarding headers
data HeaderOutputMessage =
    -- Send the headers of the principal stream
    NormalResponse_HM (GlobalStreamId, MVar HeadersSent, Headers, Effect)
    -- Send a push-promise
    |PushPromise_HM   (GlobalStreamId, GlobalStreamId, Headers, Effect)
    -- Send a reset stream notification for the stream given below
    -- --|ResetStream_HM   (GlobalStreamId, Effect)
    -- Send a GoAway, where last-stream is the stream given below.
    |GoAway_HM (GlobalStreamId, Effect)


-- Settings imposed by the peer
data SessionSettings = SessionSettings {
    _pushEnabled_SeS :: DIO.IORef Bool
  , _frameSize_SeS   :: DIO.IORef Int
    }

makeLenses ''SessionSettings



-- Whatever a worker thread is going to need comes here....
-- this is to make refactoring easier, but not strictly needed.
data WorkerThreadEnvironment = WorkerThreadEnvironment {
    -- What's the header stream id?
    _streamId                     :: GlobalStreamId

    -- A full block of headers can come here... the mvar in the middle should
    -- be populate to signal end of headers transmission. A thread will be suspended
    -- waiting for that
    , _headersOutput              :: Chan HeaderOutputMessage

    -- And regular contents can come this way and thus be properly mixed
    -- with everything else.... for now...
    ,_dataOutput                  :: MVar DataOutputToConveyor

    ,_streamsCancelled_WTE        :: MVar NS.IntSet

    ,_sessionSettings_WTE         :: SessionSettings

    ,_nextPushStream_WTE          :: MVar Int

    ,_resetStreamButton_WTE       :: IO ()

    ,_childResetStreamButton_WTE  :: GlobalStreamId -> IO ()
    }

makeLenses ''WorkerThreadEnvironment


-- An HTTP/2 session. Basically a couple of channels ...
type Session = (SessionInput, SessionOutput)


-- From outside, one can only write to this one ... the newtype is to enforce
--    this.
newtype SessionInput = SessionInput ( Chan SessionInputCommand )
sendMiddleFrameToSession :: SessionInput  -> InputFrame -> IO ()
sendMiddleFrameToSession (SessionInput chan) frame = writeChan chan $ MiddleFrame_SIC frame

sendFirstFrameToSession :: SessionInput -> InputFrame -> IO ()
sendFirstFrameToSession (SessionInput chan) frame = writeChan chan $ FirstFrame_SIC frame

sendCommandToSession :: SessionInput  -> SessionInputCommand -> IO ()
sendCommandToSession (SessionInput chan) command = writeChan chan command

type SessionOutputChannelAbstraction = Chan TT.SessionOutputPacket

-- From outside, one can only read from this one
newtype SessionOutput = SessionOutput SessionOutputChannelAbstraction
getFrameFromSession :: SessionOutput -> IO TT.SessionOutputPacket
getFrameFromSession (SessionOutput chan) = readChan chan


type HashTable k v = H.CuckooHashTable k v


type Stream2HeaderBlockFragment = HashTable GlobalStreamId Bu.Builder


type WorkerMonad = ReaderT WorkerThreadEnvironment IO


-- Have to figure out which are these...but I would expect to have things
-- like unexpected aborts here in this type.
data SessionInputCommand =
    FirstFrame_SIC InputFrame               -- This frame is special
    |MiddleFrame_SIC InputFrame             -- Ordinary frame
    |InternalAbort_SIC                      -- Internal abort from the session itself
    |InternalAbortStream_SIC GlobalStreamId  -- Internal abort, but only for a frame
    |CancelSession_SIC                      -- Cancel request from the framer
  deriving Show



-- The role of a session is either server or client. There are small
-- differences between both.
data SessionRole  =
    Client_SR
   |Server_SR
  deriving (Eq,Show)

-- Here is how we make a session
type SessionMaker = SessionsContext -> IO Session


-- Here is how we make a session wrapping a CoherentWorker
type CoherentSession = AwareWorker -> SessionMaker

data PostInputMechanism = PostInputMechanism (MVar (Maybe B.ByteString), InputDataStream)

------------- Regarding client state
type Message = (Headers,InputDataStream)

type RequestResult = Either ConnectionCloseReason Message

data ClientState = ClientState {
    -- Holds a queue. The client here puts the message (the request) and an VAR
    -- where it will receive the response.
    _pendingRequests_ClS       :: MVar (Message, MVar RequestResult)

    -- This is the id of the next available stream
    ,_nextStream_ClS           :: MVar Int

    -- Says if the client has been closed
    ,_clientIsClosed_ClS      :: MVar Bool

    -- A dictionary from stream id to the client which is waiting for the
    -- message
    ,_response2Waiter_ClS      ::HashTable GlobalStreamId (MVar RequestResult)
    }

makeLenses ''ClientState


makeClientState :: IO ClientState
makeClientState = do
    request_chan <- newEmptyMVar
    next_stream_mvar <- newMVar 3
    client_is_closed_mvar <- newMVar False
    new_h <- H.new

    return ClientState {
        _pendingRequests_ClS = request_chan
        ,_nextStream_ClS = next_stream_mvar
        ,_response2Waiter_ClS = new_h
        ,_clientIsClosed_ClS = client_is_closed_mvar
        }


handleRequest' :: ClientState -> Headers -> InputDataStream -> IO (Headers,InputDataStream)
handleRequest' client_state headers input_data = runReaderT (handleRequest headers input_data) client_state

type ClientMonad = ReaderT ClientState IO

handleRequest :: Headers -> InputDataStream -> ClientMonad Message
handleRequest headers input_data = do
    pending_requests <- view pendingRequests_ClS
    response_mvar <- liftIO $ newEmptyMVar

    liftIO $ E.catch
       (do
           {-# SCC cause1 #-} putMVar pending_requests ((headers,input_data),response_mvar)
           either_reason_or_message <- {-# SCC cause2 #-} liftIO $ takeMVar response_mvar
           case either_reason_or_message of
               Left break_reason -> E.throw $ ClientSessionAbortedException break_reason
               Right message -> return message
       )
       ( ( \ _ -> E.throw $ ClientSessionAbortedException SessionAlreadyClosed_CCR ):: E.BlockedIndefinitelyOnMVar -> IO Message )




instance ClientPetitioner ClientState where

    request = handleRequest'

-------------- end of Regarding client state

-- SessionData is the actual state of the session, including the channels to the framer
-- outside.
--
-- NH2.Frame != Frame
data SessionData = SessionData {
    -- ATTENTION: Ignore the warning coming from here for now
    _sessionsContext             :: SessionsContext

    ,_sessionInput               :: Chan SessionInputCommand

    -- We need to lock this channel occassionally so that we can order multiple
    -- header frames properly....that's the reason for the outer MVar
    ,_sessionOutput              :: MVar SessionOutputChannelAbstraction

    -- Use to encode
    ,_toEncodeHeaders            :: MVar HP.DynamicTable

    -- And used to decode
    ,_toDecodeHeaders            :: MVar HP.DynamicTable

    -- While I'm receiving headers, anything which
    -- is not a header should end in the connection being
    -- closed
    ,_receivingHeaders           :: MVar (Maybe Int)

    -- _lastGoodStream is used both to report the last good stream in the
    -- GoAwayFrame and to keep track of streams oppened by the client. In
    -- other words, it contains the stream_id of the last valid client
    -- stream and is updated as soon as the first frame of that stream is
    -- received.
    ,_lastGoodStream             :: MVar GlobalStreamId

    -- Used for decoding the headers
    ,_stream2HeaderBlockFragment :: Stream2HeaderBlockFragment

    -- Used for worker threads... this is actually a pre-filled template
    -- I make copies of it in different contexts, and as needed.
    ,_forWorkerThread            :: WorkerThreadEnvironment

    -- When acting as a server, this is the handler for processing requests
    ,_awareWorker                :: AwareWorker

    -- When acting as a client, this is where new requests are taken
    -- from
    ,_simpleClient               :: ClientState

    -- Some streams may be cancelled
    ,_streamsCancelled           :: MVar NS.IntSet

    -- Data input mechanism corresponding to some threads
    ,_stream2PostInputMechanism  :: HashTable Int PostInputMechanism

    -- Worker thread register. This is a dictionary from stream id to
    -- the ThreadId of the thread with the worker thread. I use this to
    -- raise asynchronous exceptions in the worker thread if the stream
    -- is cancelled by the client. This way we get early finalization.
    ,_stream2WorkerThread        :: HashTable Int ThreadId

    -- Use to retrieve/set the session id
    ,_sessionIdAtSession         :: Int

    -- And used to keep peer session settings
    ,_sessionSettings            :: SessionSettings

    -- What is the next stream available for push?
    ,_nextPushStream             :: MVar Int

    -- What role does this session has?
    ,_sessionRole                :: SessionRole

    }


makeLenses ''SessionData


http2ServerSession :: AwareWorker -> Int -> SessionsContext -> IO Session
http2ServerSession a i sctx = http2Session Server_SR a (error "NotAClient") i sctx


http2ClientSession :: ClientState -> Int -> SessionsContext -> IO Session
http2ClientSession client_state session_id sctx =
    http2Session Client_SR (error "NotAServer") client_state session_id sctx


--                                v- {headers table size comes here!!}
http2Session :: SessionRole -> AwareWorker -> ClientState  -> Int -> SessionsContext -> IO Session
http2Session session_role aware_worker client_state session_id sessions_context =   do
    session_input             <- newChan
    session_output            <- newChan
    session_output_mvar       <- newMVar session_output


    -- For incremental construction of headers...
    stream_request_headers    <- H.new :: IO Stream2HeaderBlockFragment

    -- Warning: we should find a way of coping with different table sizes.
    decode_headers_table      <- HP.newDynamicTableForDecoding 4096
    decode_headers_table_mvar <- newMVar decode_headers_table

    encode_headers_table      <- HP.newDynamicTableForEncoding 4096
    encode_headers_table_mvar <- newMVar encode_headers_table

    -- These ones need independent threads taking care of sending stuff
    -- their way...
    headers_output            <- newChan :: IO (Chan HeaderOutputMessage)
    data_output               <- newEmptyMVar :: IO (MVar DataOutputToConveyor)

    stream2postinputmechanism <- H.new
    stream2workerthread       <- H.new
    last_good_stream_mvar     <- newMVar (-1)

    receiving_headers         <- newMVar Nothing
    frame_size_ioref          <- DIO.newIORef (sessions_context ^.  sessionsConfig . dataFrameSize)
    push_enabled_ioref        <- DIO.newIORef (sessions_context ^.  sessionsConfig . pushEnabled)
    let
        session_settings = SessionSettings {
            _pushEnabled_SeS = push_enabled_ioref
          , _frameSize_SeS   = frame_size_ioref
            }
    next_push_stream          <- newMVar (sessions_context ^. sessionsConfig . firstPushStream )

    -- What about stream cancellation?
    cancelled_streams_mvar    <- newMVar $ NS.empty :: IO (MVar NS.IntSet)

    let for_worker_thread = WorkerThreadEnvironment {
        _streamId = error "NotInitialized"
        ,_headersOutput = headers_output
        ,_dataOutput = data_output
        ,_streamsCancelled_WTE = cancelled_streams_mvar
        ,_sessionSettings_WTE = session_settings
        ,_nextPushStream_WTE = next_push_stream
        ,_resetStreamButton_WTE = error "Not initialized"
        ,_childResetStreamButton_WTE = error "Not initialized"
        }

    let session_data  = SessionData {
        _sessionsContext             = sessions_context
        ,_sessionInput               = session_input
        ,_sessionOutput              = session_output_mvar
        ,_toDecodeHeaders            = decode_headers_table_mvar
        ,_toEncodeHeaders            = encode_headers_table_mvar
        ,_stream2HeaderBlockFragment = stream_request_headers
        ,_forWorkerThread            = for_worker_thread
        ,_awareWorker                = aware_worker
        ,_simpleClient               = client_state
        ,_streamsCancelled           = cancelled_streams_mvar
        ,_stream2PostInputMechanism  = stream2postinputmechanism
        ,_stream2WorkerThread        = stream2workerthread
        ,_sessionIdAtSession         = session_id
        ,_receivingHeaders           = receiving_headers
        ,_sessionSettings            = session_settings
        ,_lastGoodStream             = last_good_stream_mvar
        ,_nextPushStream             = next_push_stream
        ,_sessionRole                = session_role
        }

    let
        io_exc_handler :: SessionComponent -> E.BlockedIndefinitelyOnMVar -> IO ()
        io_exc_handler _component _e = do
           case session_role of
               Server_SR -> do
                   -- sessionExceptionHandler component session_id sessions_context e
                   -- Just ignore this kind of exceptions here, very explicitly, as they should naturally
                   -- happen when the framer is closed
                   return ()

               Client_SR -> do
                   clientSideTerminate client_state IOChannelClosed_CCR


        exc_handler :: SessionComponent -> HTTP2SessionException -> IO ()
        exc_handler component e = do
            case session_role of
                Server_SR ->
                    sessionExceptionHandler component session_id sessions_context e

                Client_SR -> do
                    let
                        maybe_client_session_aborted :: Maybe ClientSessionAbortedException
                        maybe_client_session_aborted | HTTP2SessionException ee <- e  = cast ee

                        maybe_protocol_error :: Maybe HTTP2ProtocolException
                        maybe_protocol_error | HTTP2SessionException ee <- e = cast ee

                    case maybe_client_session_aborted of
                        Just (ClientSessionAbortedException reason) -> do
                            clientSideTerminate client_state reason


                        Nothing ->
                            if isJust maybe_protocol_error
                                then
                                    clientSideTerminate client_state ProtocolError_CCR
                                else do
                                    E.throw e

        exc_guard :: SessionComponent -> IO () -> IO ()
        exc_guard component action =
                E.catch
                    (E.catch
                         action
                         (\e -> do
                             -- INSTRUMENTATION( errorM "HTTP2.Session" "Exception processed" )
                             exc_handler component e
                         )
                    )
                    (io_exc_handler component)

    -- Create an input thread that decodes frames...
    _ <- forkIOExc "s2f1" $ exc_guard SessionInputThread_HTTP2SessionComponent
           $ runReaderT sessionInputThread session_data

    -- Create a thread that captures headers and sends them down the tube
    _ <- forkIOExc "s2f2" $ exc_guard SessionHeadersOutputThread_HTTP2SessionComponent
           $ runReaderT (headersOutputThread headers_output session_output_mvar) session_data

    -- Create a thread that captures data and sends it down the tube. This is waiting for
    -- stuff coming from all the workers. This function is also in charge of closing streams
    _ <- forkIOExc "s2f3" $ exc_guard SessionDataOutputThread_HTTP2SessionComponent
           $ dataOutputThread frame_size_ioref data_output session_output_mvar

    -- If I'm a client, I also need a thread to poll for requests
    when (session_role == Client_SR) $ do
         _ <- forkIOExc "s2f4" $ exc_guard SessionClientPollThread_HTTP2SessionComponent
                $ sessionPollThread session_data headers_output
         return ()


    -- The two previous threads fill the session_output argument below (they write to it)
    -- the session machinery in the other end is in charge of sending that data through the
    -- socket.

    return ( (SessionInput session_input),
             (SessionOutput session_output) )


-- TODO: Some ill clients can break this thread with exceptions. Make these paths a bit
--- more robust.
sessionInputThread :: ReaderT SessionData IO ()
sessionInputThread  = do
    -- This is an introductory and declarative block... all of this is tail-executed
    -- every time that  a packet needs to be processed. It may be a good idea to abstract
    -- these values in a closure...
    session_input             <- view sessionInput

    -- decode_headers_table_mvar <- view toDecodeHeaders
    -- stream_request_headers    <- view stream2HeaderBlockFragment
    cancelled_streams_mvar    <- view streamsCancelled
    -- coherent_worker           <- view awareWorker

    -- for_worker_thread_uns     <- view forWorkerThread
    stream2workerthread       <- view stream2WorkerThread
    -- receiving_headers_mvar    <- view receivingHeaders
    -- last_good_stream_mvar     <- view lastGoodStream
    -- current_session_id        <- view sessionIdAtSession

    input                     <- {-# SCC session_input #-} liftIO $ readChan session_input
    session_role              <- view sessionRole

    case input of

        FirstFrame_SIC (NH2.Frame
            (NH2.FrameHeader _ 1 null_stream_id ) _ )| 0 == null_stream_id  -> do
            -- This is a SETTINGS ACK frame, which is okej to have,
            -- do nothing here
            continue

        FirstFrame_SIC
            (NH2.Frame
                (NH2.FrameHeader _ 0 null_stream_id )
                (NH2.SettingsFrame settings_list)
            ) | 0 == null_stream_id  -> do
            -- Good, handle
            handleSettingsFrame settings_list
            continue

        FirstFrame_SIC _ -> do
            -- Bad, incorrect id or god knows only what ....
            closeConnectionBecauseIsInvalid NH2.ProtocolError
            return ()

        CancelSession_SIC -> do
            -- Good place to tear down worker threads... Let the rest of the finalization
            -- to the framer.
            --
            -- This message is normally got from the Framer
            liftIO $ do
                H.mapM_
                    (\ (_, thread_id) -> do
                        throwTo thread_id StreamCancelledException
                        return ()
                    )
                    stream2workerthread

            -- We do not continue here, but instead let it finish
            return ()

        InternalAbort_SIC -> do
            -- Message triggered because the worker failed to behave.
            -- When this is sent, the connection is closed
            closeConnectionBecauseIsInvalid NH2.InternalError
            return ()

        InternalAbortStream_SIC stream_id -> do
            -- Message triggered because the worker failed to behave, but
            -- here we believe that it is not necessary to tear down the
            -- entire session and therefore it's enough with a stream
            -- reset
            sendOutPriorityTrain
                (NH2.EncodeInfo
                    NH2.defaultFlags
                    stream_id
                    Nothing
                )
                (NH2.RSTStreamFrame NH2.InternalError
                )
            continue

        -- The block below will process both HEADERS and CONTINUATION frames.
        -- TODO: As it stands now, the server will happily start a new stream with
        -- a CONTINUATION frame instead of a HEADERS frame. That's against the
        -- protocol.
        MiddleFrame_SIC frame | Just (_stream_id, _bytes) <- isAboutHeaders frame ->
            case session_role of
                Server_SR -> do
                    -- Just append the frames to streamRequestHeaders
                    serverProcessIncomingHeaders frame
                    continue

                Client_SR -> do
                    clientProcessIncomingHeaders frame
                    continue

        -- Peer can order a stream aborted, meaning that we shall not send any more data
        -- on it.
        MiddleFrame_SIC frame@(NH2.Frame _ (NH2.RSTStreamFrame _error_code_id)) -> do
            let stream_id = streamIdFromFrame frame
            liftIO $ do
                putStrLn $ "StreamReset " ++ show (_error_code_id)
                cancelled_streams <- takeMVar cancelled_streams_mvar
                putMVar cancelled_streams_mvar $ NS.insert  stream_id cancelled_streams
            closePostDataSource stream_id
            liftIO $ do
                maybe_thread_id <- H.lookup stream2workerthread stream_id
                case maybe_thread_id  of
                    Nothing ->
                        -- This is can actually happen in some implementations: we are asked to
                        -- cancel an stream we know nothing about.
                        return ()

                    Just thread_id -> do
                        -- INSTRUMENTATION( infoM "HTTP2.Session" $ "Stream successfully interrupted" )
                        throwTo thread_id StreamCancelledException
                        H.delete stream2workerthread stream_id

            continue

        MiddleFrame_SIC frame@(NH2.Frame (NH2.FrameHeader _ _ nh2_stream_id) (NH2.DataFrame somebytes))
          -> unlessReceivingHeaders $ do
            -- So I got data to process
            -- TODO: Handle end of stream
            let stream_id = nh2_stream_id
            -- TODO: Handle the cases where the stream_id doesn't match an already existent
            -- stream. In such cases it is justified to reset the connection with a  protocol_error.

            was_ok <- streamWorkerSendData stream_id somebytes

            if was_ok
              then do
                -- After that data has been received and forwarded downstream, we can issue a windows update
                --
                --
                -- TODO: Consider that the best place to output these frames can be somewhere else...
                --
                -- TODO: Use a special, with-quota queue here to do flow control. Don't send meaningless
                --       WindowUpdateFrame's
                sendOutPriorityTrainMany [
                      (
                        (NH2.EncodeInfo
                            NH2.defaultFlags
                            nh2_stream_id
                            Nothing
                        ),
                        (NH2.WindowUpdateFrame
                            (fromIntegral (B.length somebytes))
                        )
                      ),

                      (
                        (NH2.EncodeInfo
                            NH2.defaultFlags
                            0
                            Nothing
                        ),
                        (NH2.WindowUpdateFrame
                            (fromIntegral (B.length somebytes))
                        )
                      )
                    ]

                if frameEndsStream frame
                  then do
                    -- Good place to close the source ...
                    closePostDataSource stream_id
                  else
                    return ()

                continue
              else do
                -- For some reason there is no PostInput processing mechanism, therefore,
                -- we were not expecting data at this point
                closeConnectionBecauseIsInvalid NH2.ProtocolError
                return ()


        MiddleFrame_SIC (NH2.Frame (NH2.FrameHeader _ flags _) (NH2.PingFrame _)) | NH2.testAck flags-> do
            -- Deal with pings: this is an Ack, so do nothing
            continue

        MiddleFrame_SIC (NH2.Frame (NH2.FrameHeader _ _ _) (NH2.PingFrame somebytes))  -> do
            -- Deal with pings: NOT an Ack, so answer
            -- INSTRUMENTATION( debugM "HTTP2.Session" "Ping processed" )
            sendOutPriorityTrain
                (NH2.EncodeInfo
                    (NH2.setAck NH2.defaultFlags)
                    0
                    Nothing
                )
                (NH2.PingFrame somebytes)


            continue

        MiddleFrame_SIC (NH2.Frame frame_header (NH2.SettingsFrame _)) | isSettingsAck frame_header -> do
            -- Frame was received by the peer, do nothing here...
            continue

        -- TODO: Do something with these settings!!
        MiddleFrame_SIC (NH2.Frame _ (NH2.SettingsFrame settings_list))  -> do
            -- INSTRUMENTATION( debugM "HTTP2.Session" $ "Received settings: " ++ (show settings_list) )
            -- Just acknowledge the frame.... for now
            handleSettingsFrame settings_list
            continue

        MiddleFrame_SIC (NH2.Frame _ (NH2.GoAwayFrame _ _ _ ))
            | Server_SR <- session_role -> do
                -- I was sent a go away, so go-away...
                closeConnectionBecauseIsInvalid NH2.NoError
                return ()

            | Client_SR <- session_role -> do
                -- I was sent a go away, so go-away, but use the kind of exception
                -- that will unwind the stack gracefully
                _ <- closeConnectionForClient NH2.NoError
                return ()


        MiddleFrame_SIC _somethingelse ->  unlessReceivingHeaders $ do
            -- An undhandled case here....
            continue

  where
    continue = sessionInputThread

    -- TODO: Do use the settings!!!
    handleSettingsFrame :: NH2.SettingsList -> ReaderT SessionData IO ()
    handleSettingsFrame _settings_list = do
        session_settings <- view sessionSettings
        sessions_config <- view $ sessionsContext . sessionsConfig
        let
            enable_push = lookup NH2.SettingsEnablePush _settings_list
            max_frame_size = lookup NH2.SettingsMaxFrameSize _settings_list

        case enable_push of
            Just 1      -> liftIO $ DIO.writeIORef (session_settings ^. pushEnabled_SeS) True
            Just 0      -> liftIO $ DIO.writeIORef (session_settings ^. pushEnabled_SeS) False
            Just _      ->  closeConnectionBecauseIsInvalid NH2.ProtocolError

            Nothing     ->  return ()

        case max_frame_size of
            -- The spec says clearly what's the minimum size that can come here
            Just n | n < 16384 || n > 16777215
                        -> do
                           -- liftIO $ putStrLn "Wild max frame size"
                           closeConnectionBecauseIsInvalid NH2.ProtocolError
                   | otherwise
                        ->
                           if n > (sessions_config ^. dataFrameSize)
                              -- Ignore if it is bigger than the size configured in this context
                              then do
                                 -- liftIO . putStrLn $ "n= " ++ show n
                                 return ()
                              else
                                  liftIO $ do
                                      -- putStrLn $ "n= " ++ show n
                                      DIO.writeIORef (session_settings ^. frameSize_SeS) n

            Nothing     -> return ()

        sendOutPriorityTrain
            (NH2.EncodeInfo
                (NH2.setAck NH2.defaultFlags)
                0
                Nothing
            )
            (NH2.SettingsFrame [])


serverProcessIncomingHeaders :: NH2.Frame ->  ReaderT SessionData IO ()
serverProcessIncomingHeaders frame | Just (stream_id, bytes) <- isAboutHeaders frame = do

    -- Just append the frames to streamRequestHeaders
    opens_stream              <- appendHeaderFragmentBlock stream_id bytes
    receiving_headers_mvar    <- view receivingHeaders
    last_good_stream_mvar     <- view lastGoodStream
    for_worker_thread_uns     <- view forWorkerThread
    decode_headers_table_mvar <- view toDecodeHeaders
    stream_request_headers    <- view stream2HeaderBlockFragment
    coherent_worker           <- view awareWorker
    current_session_id        <- view sessionIdAtSession
    session_input             <- view sessionInput
    stream2workerthread       <- view stream2WorkerThread

    if opens_stream
      then {-# SCC gpAb #-} do
        maybe_rcv_headers_of <- liftIO $ takeMVar receiving_headers_mvar
        case maybe_rcv_headers_of of
          Just _ -> do
            -- Bad peer, it is already sending headers
            -- and trying to open another one
            {-# SCC ccB1 #-} closeConnectionBecauseIsInvalid NH2.ProtocolError
            -- An exception will be thrown above, so to not complicate
            -- control flow here too much.
          Nothing -> do
            -- Signal that we are receiving headers now, for this stream
            liftIO $ putMVar receiving_headers_mvar (Just stream_id)
            -- And go to check if the stream id is valid
            last_good_stream <- liftIO $ takeMVar last_good_stream_mvar
            if (odd stream_id ) && (stream_id > last_good_stream)
              then do
                -- We are golden, set the new good stream
                liftIO $ putMVar last_good_stream_mvar (stream_id)
              else do
                -- We are not golden
                -- INSTRUMENTATION( errorM "HTTP2.Session" "Protocol error: bad stream id")
                {-# SCC ccB2 #-} closeConnectionBecauseIsInvalid NH2.ProtocolError
      else {-# SCC gpcb #-} do
        maybe_rcv_headers_of <- liftIO $ takeMVar receiving_headers_mvar
        case maybe_rcv_headers_of of
            Just a_stream_id
              | a_stream_id == stream_id -> do
                  -- Nothing to complain about
                  liftIO $ putMVar receiving_headers_mvar maybe_rcv_headers_of
              | otherwise ->
                  error "IncorrectStreamId1"

            Nothing -> error "InternalError, this should be set"

    if frameEndsHeaders frame then {-# SCC gpNNAk #-}
      do
        -- Ok, let it be known that we are not receiving more headers
        liftIO $ modifyMVar_
            receiving_headers_mvar
            (\ _ -> return Nothing )
        -- Lets get a time
        headers_arrived_time      <- liftIO $ getTime Monotonic
        -- Let's decode the headers
        let
            reset_button  = writeChan session_input (InternalAbortStream_SIC stream_id)
            child_reset_button = \stream_id' -> writeChan session_input  (InternalAbortStream_SIC stream_id')
            -- Prepare the environment for the new working thread
            for_worker_thread     =
                (set streamId stream_id)
                .
                (set resetStreamButton_WTE reset_button)
                .
                (set childResetStreamButton_WTE child_reset_button)
                $
                for_worker_thread_uns
        headers_bytes             <- getHeaderBytes stream_id
        dyn_table                 <- liftIO $ takeMVar decode_headers_table_mvar
        (new_table, header_list ) <- liftIO $ HP.decodeHeader dyn_table headers_bytes
        -- /DEBUG
        -- Good moment to remove the headers from the table.... we don't want a space
        -- leak here
        liftIO $ do
            H.delete stream_request_headers stream_id
            putMVar decode_headers_table_mvar new_table

        -- TODO: Validate headers, abort session if the headers are invalid.
        -- Otherwise other invariants will break!!
        -- THIS IS PROBABLY THE BEST PLACE FOR DOING IT.
        let
            headers_editor = He.fromList header_list

        maybe_good_headers_editor <- validateIncomingHeadersServer headers_editor

        good_headers <- case maybe_good_headers_editor of
            Just yes_they_are_good -> return yes_they_are_good
            Nothing -> {-# SCC ccB3 #-} do
                closeConnectionBecauseIsInvalid NH2.ProtocolError
                return . error $ "NotUsedHeaderRepr"

        -- Add any extra headers, on demand
        --headers_extra_good      <- addExtraHeaders good_headers
        let
            headers_extra_good = good_headers
            header_list_after = He.toList headers_extra_good
        -- liftIO $ putStrLn $ "header list after " ++ (show header_list_after)

        -- If the headers end the request....
        post_data_source <- if not (frameEndsStream frame)
          then do
            mechanism <- createMechanismForStream stream_id
            let source = postDataSourceFromMechanism mechanism
            return $ Just source
          else do
            return Nothing

        let
          perception = Perception {
              _startedTime_Pr = headers_arrived_time,
              _streamId_Pr = stream_id,
              _sessionId_Pr = current_session_id,
              _protocol_Pr = Http2_HPV,
              _anouncedProtocols_Pr = Nothing
              }
          request' = Request {
              _headers_RQ = header_list_after,
              _inputData_RQ = post_data_source,
              _perception_RQ = perception
              }

        -- TODO: Handle the cases where a request tries to send data
        -- even if the method doesn't allow for data.

        -- I'm clear to start the worker, in its own thread
        --
        -- NOTE: Some late internal errors from the worker thread are
        --       handled here by closing the session.
        --
        -- TODO: Log exceptions handled here.

        liftIO $ do
            -- The mvar below: avoid starting until the entry has
            -- been properly inserted in the table...
            ready <- newMVar ()
            thread_id <- forkIOExc "s2f7" $ E.catch
                ({-# SCC growP1 #-} do
                    putMVar ready ()
                    runReaderT
                        (workerThread
                               request'
                               coherent_worker)
                        for_worker_thread
                    H.delete stream2workerthread stream_id
                )
                (
                    (   \ (E.SomeException e) ->  do
                        -- Actions to take when the thread breaks....
                         -- We cancel the entire session because there is a more specific
                        -- handler that doesn't somewhere below. If the exception bubles here,
                        -- it is because the situation is out of control. We may as well
                        -- exit the server, but I'm not being so extreme now.
                        H.delete stream2workerthread stream_id
                        putStrLn $ "ERROR: Aborting session after non-handled exception bubled up " ++ E.displayException e
                        writeChan session_input InternalAbort_SIC
                    )
                    :: E.SomeException -> IO ()
                )
            H.insert stream2workerthread stream_id thread_id
            takeMVar ready



        return ()
    else
        -- Frame doesn't end the headers... it was added before... so
        -- probably do nothing
        return ()

serverProcessIncomingHeaders _ = error "serverProcessIncomingHeadersNotDefined"

clientProcessIncomingHeaders :: NH2.Frame ->  ReaderT SessionData IO ()
clientProcessIncomingHeaders frame | Just (stream_id, bytes) <- isAboutHeaders frame = do

    opens_stream              <- appendHeaderFragmentBlock stream_id bytes
    receiving_headers_mvar    <- view receivingHeaders
    last_good_stream_mvar     <- view lastGoodStream
    decode_headers_table_mvar <- view toDecodeHeaders
    stream_request_headers    <- view stream2HeaderBlockFragment
    response2waiter           <- view (simpleClient . response2Waiter_ClS)

    if opens_stream
      then do
        maybe_rcv_headers_of <- liftIO $ takeMVar receiving_headers_mvar
        case maybe_rcv_headers_of of
          Just _ -> do
            -- Bad peer, it is already sending headers
            -- and trying to open another one
            closeConnectionBecauseIsInvalid NH2.ProtocolError
            -- An exception will be thrown above, so to not complicate
            -- control flow here too much.
          Nothing -> do
            -- Signal that we are receiving headers now, for this stream
            liftIO $ putMVar receiving_headers_mvar (Just stream_id)
            -- And go to check if the stream id is valid
            --last_good_stream <- liftIO $ takeMVar last_good_stream_mvar
            stream_initiated_by_client <- streamInitiatedByClient stream_id
            if (odd stream_id ) && stream_initiated_by_client
              then do
                -- We are golden, set the new good stream
                liftIO $ putMVar last_good_stream_mvar (stream_id)
              else do
                -- We are not golden
                -- TODO: Control for pushed streams here.
                closeConnectionBecauseIsInvalid NH2.ProtocolError
      else do
        maybe_rcv_headers_of <- liftIO $ takeMVar receiving_headers_mvar
        case maybe_rcv_headers_of of
            Just a_stream_id
              | a_stream_id == stream_id -> do
                   -- Nothing to complain about
                   liftIO $ putMVar receiving_headers_mvar maybe_rcv_headers_of
              | otherwise -> error "StreamIdMismatch3"

            Nothing -> error "InternalError, this should be set"

    if frameEndsHeaders frame then
      do
        -- Ok, let it be known that we are not receiving more headers
        liftIO $ modifyMVar_
            receiving_headers_mvar
            (\ _ -> return Nothing )
        -- Lets get a time
        -- headers_arrived_time      <- liftIO $ getTime Monotonic
        -- Let's decode the headers
        headers_bytes             <- getHeaderBytes stream_id
        dyn_table                 <- liftIO $ takeMVar decode_headers_table_mvar
        (new_table, header_list ) <- liftIO $ HP.decodeHeader dyn_table headers_bytes

        -- Good moment to remove the headers from the table.... we don't want a space
        -- leak here
        liftIO $ do
            H.delete stream_request_headers stream_id
            putMVar decode_headers_table_mvar new_table

        -- TODO: Validate headers, abort session if the headers are invalid.
        -- Otherwise other invariants will break!!
        -- THIS IS PROBABLY THE BEST PLACE FOR DOING IT.
        let
            headers_editor = He.fromList header_list

        maybe_good_headers_editor <- validateIncomingHeadersClient headers_editor

        good_headers <- case maybe_good_headers_editor of
            Just yes_they_are_good -> return yes_they_are_good
            -- Function below throws an exception and therefore closes everything
            -- TODO: Can we device smoother ways of terminating the session?
            Nothing -> do
                closeConnectionBecauseIsInvalid NH2.ProtocolError
                return . error $ "NotUsedHeaderRepr2"

        -- If the headers end the request....
        post_data_source <- if not (frameEndsStream frame)
          then do
            mechanism <-  createMechanismForStream stream_id
            return $ postDataSourceFromMechanism mechanism
          else
            return $ return ()

        (Just response_mvar) <- liftIO $ H.lookup response2waiter stream_id
        liftIO $ putMVar response_mvar $ Right (He.toList good_headers, post_data_source)

        return ()
    else
        -- Frame doesn't end the headers... it was added before... so
        -- probably do nothing
        return ()

clientProcessIncomingHeaders _ = error "Not defined"

streamInitiatedByClient :: GlobalStreamId -> ReaderT SessionData IO Bool
streamInitiatedByClient stream_id = do
    response2waiter <- view (simpleClient . response2Waiter_ClS)
    response_handle_maybe <- liftIO $ H.lookup response2waiter stream_id
    return $ isJust response_handle_maybe


sendOutPriorityTrain :: NH2.EncodeInfo -> NH2.FramePayload  -> ReaderT SessionData IO ()
sendOutPriorityTrain encode_info payload = do
    session_output_mvar <- view sessionOutput

    session_output <- liftIO $ takeMVar session_output_mvar
    liftIO $ writeChan session_output $ TT.PriorityTrain_StFB [(
      encode_info,
      payload,
      -- Not sending effects in this frame, since it is not related...
      error "sendOutFrameNotFor")]
    liftIO $ putMVar session_output_mvar session_output

sendOutPriorityTrainMany :: [(NH2.EncodeInfo , NH2.FramePayload)]  -> ReaderT SessionData IO ()
sendOutPriorityTrainMany many = do
    session_output_mvar <- view sessionOutput

    session_output <- liftIO $ takeMVar session_output_mvar
    liftIO $
        writeChan session_output  $
        TT.PriorityTrain_StFB $
        map
            (\(encode_info, payload) -> (encode_info, payload, error "no-effect"))
        many
    liftIO $ putMVar session_output_mvar session_output


-- TODO: Close connection on unexepcted pseudo-headers
validateIncomingHeadersServer :: He.HeaderEditor -> ReaderT SessionData IO (Maybe He.HeaderEditor)
validateIncomingHeadersServer headers_editor = do
    -- Check that the headers block comes with all mandatory headers.
    -- Right now I'm not checking that they come in the mandatory order though...
    --
    -- Notice that this function will transform a "host" header to an ":authority"
    -- one.
    let
        h1 = He.replaceHostByAuthority headers_editor
        -- Check that headers are lowercase
        headers_are_lowercase = He.headersAreLowercaseAtHeaderEditor headers_editor
        -- Check that we have mandatory headers
        maybe_authority = h1 ^. (He.headerLens ":authority")
        maybe_method    = h1 ^. (He.headerLens ":method")
        maybe_scheme    = h1 ^. (He.headerLens ":scheme")
        maybe_path      = h1 ^. (He.headerLens ":path")

    if
        (isJust maybe_authority) &&
        (isJust maybe_method) &&
        (isJust maybe_scheme) &&
        (isJust maybe_path ) &&
        headers_are_lowercase
        then
            return (Just h1)
        else
            return Nothing

validateIncomingHeadersClient :: He.HeaderEditor -> ReaderT SessionData IO (Maybe He.HeaderEditor)
validateIncomingHeadersClient headers_editor = do
    -- Check that the headers block comes with all mandatory headers.
    -- Right now I'm not checking that they come in the mandatory order though...
    --
    -- Notice that this function will transform a "host" header to an ":authority"
    -- one.
    let
        h1 = He.replaceHostByAuthority headers_editor
        -- Check that headers are lowercase
        headers_are_lowercase = He.headersAreLowercaseAtHeaderEditor headers_editor
        -- Check that we have mandatory headers
        maybe_status = h1 ^. (He.headerLens ":status")

    if (isJust maybe_status) && headers_are_lowercase
        then
            return (Just h1)
        else
            return Nothing


-- Sends a GO_AWAY frame and raises an exception, effectively terminating the input
-- thread of the session.
closeConnectionBecauseIsInvalid :: NH2.ErrorCodeId -> ReaderT SessionData IO ()
closeConnectionBecauseIsInvalid error_code = do
    -- liftIO $ errorM "HTTP2.Session" "closeConnectionBecauseIsInvalid called!"
    last_good_stream_mvar <- view lastGoodStream
    last_good_stream <- liftIO $ takeMVar last_good_stream_mvar
    session_output_mvar <- view sessionOutput
    stream2workerthread <- view stream2WorkerThread

    _ <- liftIO $ do
        -- Close all active threads for this session
        H.mapM_
            ( \(_stream_id, thread_id) ->
                    throwTo thread_id StreamCancelledException
            )
            stream2workerthread

        -- Notify the framer that the session is closing, so
        -- that it stops accepting frames from connected sources
        -- (Streams?)
        session_output <- takeMVar session_output_mvar
        writeChan session_output $ TT.Command_StFB (TT.SpecificTerminate_SOC last_good_stream error_code)
        putMVar session_output_mvar session_output

        -- And unwind the input thread in the session, so that the
        -- exception handler runs....
        E.throw HTTP2ProtocolException

    -- It never gets here
    return ()

-- Sends a GO_AWAY frame and raises an exception, effectively terminating the input
-- thread of the session. This one for the client is different because it throws
-- an exception of type ClientSessionAbortedException
closeConnectionForClient :: NH2.ErrorCodeId -> ReaderT SessionData IO a
closeConnectionForClient error_code = do
    let
        use_reason = case error_code of
            NH2.NoError -> NormalTermination_CCR
            _           -> ProtocolError_CCR

    -- liftIO $ errorM "HTTP2.Session" "closeConnectionBecauseIsInvalid called!"
    last_good_stream_mvar <- view lastGoodStream
    last_good_stream <- liftIO $ takeMVar last_good_stream_mvar
    session_output_mvar <- view sessionOutput
    stream2workerthread <- view stream2WorkerThread
    -- sendOutFrame
    --     (NH2.EncodeInfo
    --         NH2.defaultFlags
    --         0
    --         Nothing
    --     )
    --     (NH2.GoAwayFrame
    --         last_good_stream
    --         error_code
    --         ""
    --     )

    client_is_closed_mvar <- view (simpleClient . clientIsClosed_ClS )

    liftIO $ do
        -- Close all active threads for this session
        H.mapM_
            ( \(_stream_id, thread_id) ->
                    throwTo thread_id StreamCancelledException
            )
            stream2workerthread

        -- Notify the framer that the session is closing, so
        -- that it stops accepting frames from connected sources
        -- (Streams?)
        session_output <- takeMVar session_output_mvar
        writeChan session_output . TT.Command_StFB . TT.SpecificTerminate_SOC  last_good_stream $ error_code
        putMVar session_output_mvar session_output

        -- Let's also mark the session as closed from the client side, so that
        -- any further requests end with the correct exception
        modifyMVar_ client_is_closed_mvar (\ _ -> return True)

        -- And unwind the input thread in the session, so that the
        -- exception handler runs....
        E.throw $ ClientSessionAbortedException use_reason


frameEndsStream :: InputFrame -> Bool
frameEndsStream (NH2.Frame (NH2.FrameHeader _ flags _) _)  = NH2.testEndStream flags


-- Executes its argument, unless receiving
-- headers, in which case the connection is closed.
unlessReceivingHeaders :: ReaderT SessionData IO a -> ReaderT SessionData IO a
unlessReceivingHeaders comp = do
    receiving_headers_mvar    <- view receivingHeaders
    -- First check if we are receiving headers
    maybe_recv_headers <- liftIO $ readMVar receiving_headers_mvar
    if isJust maybe_recv_headers
      then do
        -- So, this frame is highly illegal
        closeConnectionBecauseIsInvalid NH2.ProtocolError
        return . error $ "NotUsedRepr3"
      else
        comp


createMechanismForStream :: GlobalStreamId -> ReaderT SessionData IO PostInputMechanism
createMechanismForStream stream_id = do
    (chan, source) <- liftIO $ unfoldChannelAndSource
    stream2postinputmechanism <- view stream2PostInputMechanism
    let pim = PostInputMechanism (chan, source)
    liftIO $ H.insert stream2postinputmechanism stream_id pim
    return pim


-- TODO: Can be optimized by factoring out the mechanism lookup
-- TODO IMPORTANT: This is a good place to drop the postinputmechanism
-- for a stream, so that unprocessed data can be garbage-collected.
closePostDataSource :: GlobalStreamId -> ReaderT SessionData IO ()
closePostDataSource stream_id = do
    stream2postinputmechanism <- view stream2PostInputMechanism

    pim_maybe <- liftIO $ H.lookup stream2postinputmechanism stream_id

    case pim_maybe of

        Just (PostInputMechanism (chan, _))  ->
            liftIO $ do
                putMVar chan Nothing
                -- Not sure if this will work
                H.delete  stream2postinputmechanism stream_id

        Nothing ->
            -- Assume this is ok
            return ()


streamWorkerSendData :: Int -> B.ByteString -> ReaderT SessionData IO Bool
streamWorkerSendData stream_id bytes = do
    s2pim <- view stream2PostInputMechanism
    pim_maybe <- liftIO $ H.lookup s2pim stream_id

    case pim_maybe of

        Just pim  -> do
            sendBytesToPim pim bytes
            return True

        Nothing ->
            -- There is no input mechanism
            return False


sendBytesToPim :: PostInputMechanism -> B.ByteString -> ReaderT SessionData IO ()
sendBytesToPim (PostInputMechanism (chan, _)) bytes =
    liftIO $ putMVar chan (Just bytes)


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


sendPrimitive500Error :: IO TupledPrincipalStream
sendPrimitive500Error =
  return (
        [
            (":status", "500")
        ],
        [],
        do
            yield "Internal server error\n"
            -- No footers
            return []
    )


-- Invokes the Coherent worker. Data is sent through pipes to
-- the output thread here in this session.
workerThread :: Request -> AwareWorker -> WorkerMonad ()
workerThread req aware_worker =
  do
    headers_output <- view headersOutput
    stream_id      <- view streamId
    --session_settings <- view sessionSettings_WTE
    --next_push_stream_mvar <-  view nextPushStream_WTE

    -- If the request get rejected right away, we can just send
    -- a 500 error in this very stream, without making any fuss.
    -- (headers, _, data_and_conclussion)
    --
    -- TODO: Can we add debug information in a header here?
    principal_stream <-
        liftIO $ E.catch
            (
                aware_worker  req
            )
            (
                const $ tupledPrincipalStreamToPrincipalStream <$> sendPrimitive500Error
                :: HTTP500PrecursorException -> IO PrincipalStream
            )

    -- Pieces of the header
    let
       effects              = principal_stream ^. effect_PS
       interrupt_maybe      = effects          ^. interrupt_Ef


    -- Exceptions can bubble in the points below. If so, send proper resets...
    case interrupt_maybe of

        Nothing -> do
            normallyHandleStream principal_stream

        Just (InterruptConnectionAfter_IEf) -> do
            normallyHandleStream principal_stream
            liftIO . writeChan headers_output $ GoAway_HM (stream_id, effects)

        Just (InterruptConnectionNow_IEf) -> do
            -- Not one hundred-percent sure of this being correct, but we don't want
            -- to acknowledge reception of this stream then
            let use_stream_id = stream_id - 1
            liftIO . writeChan headers_output $ GoAway_HM (use_stream_id, effects)


normallyHandleStream :: PrincipalStream -> WorkerMonad ()
normallyHandleStream principal_stream = do
    headers_output <- view headersOutput
    stream_id      <- view streamId
    session_settings <- view sessionSettings_WTE
    next_push_stream_mvar <-  view nextPushStream_WTE
    reset_button <- view resetStreamButton_WTE

    -- Pieces of the header
    let
       headers              = principal_stream ^. headers_PS
       data_and_conclusion  = principal_stream ^. dataAndConclusion_PS
       effects              = principal_stream ^. effect_PS
       pushed_streams       = principal_stream ^. pushedStreams_PS

       can_push_ioref       = session_settings ^. pushEnabled_SeS

    can_push <- liftIO . DIO.readIORef $ can_push_ioref
      -- This gets executed in normal conditions, when no interruption is required.

    -- There are several possible moments where the PUSH_PROMISEs can be sent,
    -- but a default safe one is before sending the response HEADERS, so that
    -- LINK headers in the response come after any potential promises.
    data_promises <- if can_push then do
        forM pushed_streams $ \ pushed_stream_comp -> do
            -- Initialize pushed stream
            pushed_stream <- liftIO pushed_stream_comp
            -- Send the request headers properly wrapped in a "Push Promise", do
            -- it before sending the actual response headers of this stream
            let
                request_headers            = pushed_stream ^. requestHeaders_Psh
                response_headers           = pushed_stream ^. responseHeaders_Psh
                pushed_data_and_conclusion = pushed_stream ^. dataAndConclusion_Psh

            child_stream_id <- liftIO $ modifyMVar next_push_stream_mvar
                                     $ (\ x -> return (x+2,x) )

            liftIO . writeChan headers_output . PushPromise_HM $
                (stream_id, child_stream_id, request_headers, effects)
            return (child_stream_id, response_headers, pushed_data_and_conclusion, effects)
      else
        return []

    -- Now I send the headers, if that's possible at all
    headers_sent <- liftIO  newEmptyMVar
    liftIO $ writeChan headers_output $ NormalResponse_HM (stream_id, headers_sent, headers, effects)

    -- At this moment I should ask if the stream hasn't been cancelled by the browser before
    -- commiting to the work of sending addtitional data... this is important for pushed
    -- streams
    is_stream_cancelled <- isStreamCancelled stream_id
    unless  is_stream_cancelled $ do
        -- I have a beautiful source that I can de-construct...
        -- TODO: Optionally pulling data out from a Conduit ....
        -- liftIO ( data_and_conclussion $$ (_sendDataOfStream stream_id) )
        --
        -- This threadlet should block here waiting for the headers to finish going
        -- NOTE: Exceptions generated here inheriting from HTTP500PrecursorException
        -- are let to bubble and managed in this thread fork point...
        either_err_or_candies <- CMC.try . runConduit $
             transPipe liftIO data_and_conclusion
            `fuseBothMaybe`
             sendDataOfStream stream_id headers_sent effects

        case either_err_or_candies :: Either HTTP500PrecursorException (Maybe Footers, ()) of
             Left _e -> do
                 -- Exceptions can happen for a number of reason, we just
                 -- press the reset button for this stream
                 liftIO reset_button

             Right (_maybe_footers, _) ->
                 return ()

        -- BIG TODO: Send the footers ... likely stream conclusion semantics
        -- will need to be changed.

        -- Now, time to fork threads for the pusher streams
        -- we send those pushers even if the main stream is cancelled...
        forM_ data_promises
            $ \ (child_stream_id, response_headers, pushed_data_and_conclusion, _effects) -> do
                  environment <- ask
                  let
                      action = pusherThread
                                    child_stream_id
                                    response_headers
                                    pushed_data_and_conclusion
                                    effects
                  -- And let the action run in its own thread
                  liftIO . forkIOExc "s2f8" $ runReaderT action environment

        return ()



-- Takes care of pushed data, which  is sent through pipes to
-- the output thread here in this session.
pusherThread :: GlobalStreamId -> Headers -> DataAndConclusion -> Effect  -> WorkerMonad ()
pusherThread child_stream_id response_headers pushed_data_and_conclusion effects =
  do
    headers_output <- view headersOutput
    -- session_settings <- view sessionSettings_WTE
    pushed_reset_button <- view childResetStreamButton_WTE

    -- TODO: Handle exceptions here: what happens if the coherent worker
    --       throws an exception signaling that the request is ill-formed
    --       and should be dropped? That could happen in a couple of occassions,
    --       but really most cases should be handled here in this file...
    -- (headers, _, data_and_conclussion)


    -- Now I send the headers, if that's possible at all
    headers_sent <- liftIO  newEmptyMVar
    liftIO . writeChan headers_output
        $ NormalResponse_HM (child_stream_id, headers_sent, response_headers, effects)

    -- At this moment I should ask if the stream hasn't been cancelled by the browser before
    -- commiting to the work of sending addtitional data... this is important for pushed
    -- streams
    is_stream_cancelled <- isStreamCancelled child_stream_id
    unless  is_stream_cancelled $ do
        -- I have a beautiful source that I can de-construct...
        -- TODO: Optionally pulling data out from a Conduit ....
        -- liftIO ( data_and_conclussion $$ (_sendDataOfStream stream_id) )
        --
        -- This threadlet should block here waiting for the headers to finish going
        -- NOTE: Exceptions generated here inheriting from HTTP500PrecursorException
        -- are let to bubble and managed in this thread fork point...
        either_err_or_candies <- CMC.try . runConduit $
             transPipe liftIO pushed_data_and_conclusion
            `fuseBothMaybe`
             sendDataOfStream child_stream_id headers_sent effects

        case either_err_or_candies :: Either HTTP500PrecursorException (Maybe Footers, ()) of
             Left _e -> do
                 -- Exceptions can happen for a number of reasons, we just
                 -- press the reset button for this stream
                 liftIO $ pushed_reset_button child_stream_id

             Right (_maybe_footers, _) ->
                  return ()

        return ()



--                                                       v-- comp. monad.
sendDataOfStream :: GlobalStreamId -> MVar HeadersSent -> Effect -> Sink B.ByteString (ReaderT WorkerThreadEnvironment IO) ()
sendDataOfStream stream_id headers_sent effect = do
    data_output <- view dataOutput
    -- Wait for all headers sent
    _ <- liftIO $ takeMVar headers_sent
    consumer data_output
  where
    --delay = effect ^. middlePauseForDelivery_Ef
    consumer data_output = do
        maybe_bytes <- await
        case maybe_bytes of
            Nothing ->
                -- This is how we finish sending data
                liftIO $ putMVar data_output (stream_id, Nothing, effect)
            Just bytes
                | B.length bytes > 0 -> do
                    liftIO $ do
                        putMVar data_output (stream_id, Just bytes, effect)
                    consumer data_output
                | otherwise ->
                    liftIO $ putMVar data_output (stream_id, Nothing, effect)


-- Returns if the frame is the first in the stream
appendHeaderFragmentBlock :: GlobalStreamId -> B.ByteString -> ReaderT SessionData IO Bool
appendHeaderFragmentBlock global_stream_id bytes = do
    ht <- view stream2HeaderBlockFragment
    maybe_old_block <- liftIO $ H.lookup ht global_stream_id
    (new_block, new_stream) <- case maybe_old_block of

        Nothing -> do
            -- TODO: Make the commented message below more informative
            return $ (Bu.byteString bytes, True)

        Just something ->
            return $ (something `mappend` (Bu.byteString bytes), False)

    liftIO $ H.insert ht global_stream_id new_block
    return new_stream

getHeaderBytes :: GlobalStreamId -> ReaderT SessionData IO B.ByteString
getHeaderBytes global_stream_id = do
    ht <- view stream2HeaderBlockFragment
    Just bytes <- liftIO $ H.lookup ht global_stream_id
    return $ Bl.toStrict $ Bu.toLazyByteString bytes


isAboutHeaders :: InputFrame -> Maybe (GlobalStreamId, B.ByteString)
isAboutHeaders (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.HeadersFrame _ block_fragment   ) )
    = Just (stream_id, block_fragment)
isAboutHeaders (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.ContinuationFrame block_fragment) )
    = Just (stream_id, block_fragment)
isAboutHeaders _
    = Nothing


frameEndsHeaders  :: InputFrame -> Bool
frameEndsHeaders (NH2.Frame (NH2.FrameHeader _ flags _) _) = NH2.testEndHeader flags


streamIdFromFrame :: InputFrame -> GlobalStreamId
streamIdFromFrame (NH2.Frame (NH2.FrameHeader _ _ stream_id) _) = stream_id


-- TODO: Have different size for the headers..... just now going with a default size of 16 k...
-- TODO: Find a way to kill this thread....
headersOutputThread :: Chan HeaderOutputMessage  --  (GlobalStreamId, MVar HeadersSent, Headers, Effect)
                       -> MVar SessionOutputChannelAbstraction
                       -> ReaderT SessionData IO ()
headersOutputThread input_chan session_output_mvar = forever $ do

    frame_size_ioref <- view $ sessionSettings . frameSize_SeS
    use_chunk_length <- liftIO $ DIO.readIORef frame_size_ioref

    header_output_request <- {-# SCC input_chan  #-} liftIO $ readChan input_chan
    {-# SCC case_ #-} case header_output_request of
        NormalResponse_HM (stream_id, headers_ready_mvar, headers, effect)  -> do

            -- First encode the headers using the table
            encode_dyn_table_mvar <- view toEncodeHeaders

            encode_dyn_table <- liftIO $ takeMVar encode_dyn_table_mvar
            (new_dyn_table, data_to_send ) <- liftIO $ HP.encodeHeader HP.defaultEncodeStrategy encode_dyn_table headers
            liftIO $ putMVar encode_dyn_table_mvar new_dyn_table

            -- Now split the bytestring in chunks of the needed size....
            -- Note that the only way we can
            let
                bs_chunks = bytestringChunk use_chunk_length  data_to_send

            -- And send the chunks through while locking the output place....
            liftIO $ bs_chunks `deepseq` withMVar session_output_mvar $ \ session_output -> do
                    let
                        header_frames = headerFrames stream_id bs_chunks effect
                    writeChan session_output $ TT.PriorityTrain_StFB header_frames
                    putMVar headers_ready_mvar HeadersSent


        GoAway_HM (stream_id, _effect) -> do
           -- This is in charge of sending an interrupt message to the framer
           let
               message = TT.Command_StFB (TT.SpecificTerminate_SOC stream_id NH2.NoError)
           liftIO . withMVar session_output_mvar $
               (\session_output -> do
                   writeChan session_output message
               )

        PushPromise_HM (parent_stream_id, child_stream_id, promise_headers, effect) -> do

            encode_dyn_table_mvar <- view toEncodeHeaders
            encode_dyn_table <- liftIO $ takeMVar encode_dyn_table_mvar

            (new_dyn_table, data_to_send ) <- liftIO $ HP.encodeHeader HP.defaultEncodeStrategy encode_dyn_table promise_headers
            liftIO $ putMVar encode_dyn_table_mvar new_dyn_table

            -- Now split the bytestring in chunks of the needed size....
            bs_chunks <- return $! bytestringChunk use_chunk_length data_to_send

            -- And send the chunks through while locking the output place....
            liftIO $ E.bracket
                (takeMVar session_output_mvar)
                (putMVar session_output_mvar )
                (\ session_output -> do
                    let
                        pp_frames = pushPromiseFrames parent_stream_id child_stream_id bs_chunks effect
                    writeChan session_output $ TT.PriorityTrain_StFB pp_frames
                    -- No notification here... but since everyhing headers is serialized here and the next
                    -- piece of information corresponding to the child stream is a headers sequence, we shouldn't
                    -- need any other tricks here
                    )

  where

    chunksToSequence ::
          ( NH2.FrameFlags -> B.ByteString -> TT.OutputFrame)
       -> ( NH2.FrameFlags -> B.ByteString -> TT.OutputFrame)
       -> [B.ByteString]
       -> Bool
       -> [TT.OutputFrame]
    chunksToSequence
        transform_first
        _transform_middle
        (last_chunk:[])
        True    -- If it is first, and last
        =
        [transform_first (NH2.setEndHeader NH2.defaultFlags) last_chunk]
    chunksToSequence
        _transform_first
        transform_middle
        (last_chunk:[])
        False    -- It is not first, but last
        =
        [transform_middle (NH2.setEndHeader NH2.defaultFlags) last_chunk]
    chunksToSequence
        transform_first
        transform_middle
        (chunk:rest)
        True    -- If it is first, but not last
        =
        (transform_first NH2.defaultFlags chunk):(chunksToSequence transform_first transform_middle rest False)
    chunksToSequence
        transform_first
        transform_middle
        (chunk:rest)
        False    -- It is not first, and not last
        =
        (transform_middle NH2.defaultFlags chunk):(chunksToSequence transform_first transform_middle rest False)
    chunksToSequence _ _ [] _ = error "ChunkingEmptySetOfHeaders!!"

    headerFrames :: GlobalStreamId -> [B.ByteString] -> Effect -> [TT.OutputFrame]
    headerFrames stream_id chunks effect =
        chunksToSequence
            ( \ flags chunk -> (
                  NH2.EncodeInfo {
                      NH2.encodeFlags    = flags
                     ,NH2.encodeStreamId = stream_id
                     ,NH2.encodePadding  = Nothing },
                  NH2.HeadersFrame Nothing chunk,
                  effect
                  )
            )
            ( \ flags chunk -> (
                  NH2.EncodeInfo {
                      NH2.encodeFlags    = flags
                     ,NH2.encodeStreamId = stream_id
                     ,NH2.encodePadding  = Nothing },
                  NH2.ContinuationFrame chunk,
                  effect
                  )
            )
            chunks
            True

    pushPromiseFrames  :: GlobalStreamId -> GlobalStreamId -> [B.ByteString] -> Effect -> [TT.OutputFrame]
    pushPromiseFrames parent_stream_id child_stream_id chunks effect =
        chunksToSequence
            ( \ flags chunk -> (
                  NH2.EncodeInfo {
                      NH2.encodeFlags    = flags
                     ,NH2.encodeStreamId = parent_stream_id
                     ,NH2.encodePadding  = Nothing },
                  NH2.PushPromiseFrame child_stream_id chunk,
                  effect
                  )
            )
            ( \ flags chunk -> (
                  NH2.EncodeInfo {
                      NH2.encodeFlags    = flags
                     ,NH2.encodeStreamId = parent_stream_id
                     ,NH2.encodePadding  = Nothing },
                  NH2.ContinuationFrame chunk,
                  effect
                  )
            )
            chunks
            True


bytestringChunk :: Int -> B.ByteString -> [B.ByteString]
bytestringChunk len s | (B.length s) < len = [ s ]
bytestringChunk len s = h:(bytestringChunk len xs)
  where
    (h, xs) = B.splitAt len s


-- This thread is for the entire session, not for each stream. The thread should
-- die while waiting for input in a garbage-collected mvar.
-- TODO: This function does non-optimal chunking for the case where responses are
--       actually streamed.... in those cases we need to keep state for frames in
--       some other format....
-- TODO: Right now, we are transmitting an empty last frame with the end-of-stream
--       flag set. I'm afraid that the only
--       way to avoid that is by holding a frame or by augmenting the end-user interface
--       so that the user can signal which one is the last frame. The first approach
--       restricts responsiviness, the second one clutters things.
dataOutputThread :: DIO.IORef Int
                    -> MVar DataOutputToConveyor
                    -> MVar SessionOutputChannelAbstraction
                    -> IO ()
dataOutputThread payload_max_length_ioref  input_chan session_output_mvar = forever $ do
    (stream_id, maybe_contents, effect) <- takeMVar input_chan
    payload_max_length <- DIO.readIORef payload_max_length_ioref
    case maybe_contents of
        Nothing -> do
            liftIO $ do
                withLockedSessionOutput
                    (\ session_output ->  do
                           -- Write an empty data-frame with the right flags
                           writeChan session_output $ TT.DataFrame_StFB ( NH2.EncodeInfo {
                                 NH2.encodeFlags     = NH2.setEndStream NH2.defaultFlags
                                ,NH2.encodeStreamId  = stream_id
                                ,NH2.encodePadding   = Nothing },
                                NH2.DataFrame "",
                                effect
                                )
                           -- And then write an-end-of-stream command
                           writeChan session_output $ TT.Command_StFB . TT.FinishStream_SOC $ stream_id
                        )

        Just contents -> do
            -- And now just simply output it...
            let bs_chunks = bytestringChunk payload_max_length contents
            -- And send the chunks through while locking the output place....
            bs_chunks `deepseq` writeContinuations bs_chunks stream_id effect
            return ()

  where

    withLockedSessionOutput = E.bracket
        (takeMVar session_output_mvar)
        (putMVar session_output_mvar) -- <-- There is an implicit argument there!!

    writeContinuations :: [B.ByteString] -> GlobalStreamId -> Effect  -> IO ()
    writeContinuations fragments stream_id effect = do
      -- payload_max_length <- DIO.readIORef payload_max_length_ioref
      mapM_ (\ fragment ->
                  withLockedSessionOutput
                      (\ session_output -> do
                             -- TODO: This probably should be configurable or better tuned.
                             -- However, we are going to send frequent pings to keep a chart
                             -- of latency.
                             --when (B.length fragment == payload_max_length) $ do
                                 -- Force tons and tons of ping frames to see if we get less delays due to
                                 -- congestion...
                                 -- writeChan session_output $ Right (
                                 --     NH2.EncodeInfo {
                                 --         NH2.encodeFlags     = NH2.defaultFlags
                                 --         ,NH2.encodeStreamId = 0
                                 --         ,NH2.encodePadding  = Nothing
                                 --         },
                                 --     NH2.PingFrame "talk  on",
                                 --     effect )

                             writeChan session_output $ TT.DataFrame_StFB (
                                 NH2.EncodeInfo {
                                     NH2.encodeFlags     = NH2.defaultFlags
                                     ,NH2.encodeStreamId = stream_id
                                     ,NH2.encodePadding  = Nothing
                                     },
                                 NH2.DataFrame fragment,
                                 effect )
                      )
             )
             fragments


-- This function works for HTTP/2 client sessions only...
sessionPollThread :: SessionData -> Chan HeaderOutputMessage -> IO ()
sessionPollThread  session_data headers_output = do
    let
        pending_requests_mvar  = session_data ^. (simpleClient . pendingRequests_ClS)
        client_next_stream_mvar  = session_data ^. (simpleClient . nextStream_ClS)
        response2waiter       = session_data ^. (simpleClient . response2Waiter_ClS)
        for_worker_thread  = session_data ^. forWorkerThread
        session_input  = session_data ^. sessionInput

    ((headers, input_data_stream), response_mvar) <- takeMVar pending_requests_mvar

    new_stream_id <- modifyMVar client_next_stream_mvar (\ n -> return (n + 1, n) )
    let
        worker_environment = set streamId new_stream_id for_worker_thread
        effects = defaultEffects

    -- Store the response place
    new_stream_id `seq` H.insert response2waiter new_stream_id response_mvar

    -- Start by sending the headers of the request
    headers_sent <- liftIO  newEmptyMVar
    liftIO $ writeChan headers_output $ NormalResponse_HM (new_stream_id, headers_sent, headers, effects)

    _ <- liftIO . forkIOExc "s2f6" $ do
        either_e0 <- E.try $ runReaderT
            (clientWorkerThread new_stream_id effects headers_sent input_data_stream )
            worker_environment
        case either_e0 :: Either HTTP500PrecursorException () of
            Left _ -> writeChan session_input $ InternalAbortStream_SIC new_stream_id
            Right _ -> return ()

     -- E.catch
     --        (runReaderT
     --            (clientWorkerThread new_stream_id effects headers_sent input_data_stream )
     --            worker_environment
     --        )
     --        (
     --            (   \ _ ->  do
     --                -- Actions to take when the thread breaks....
     --                writeChan session_input InternalAbort_SIC
     --            )
     --            :: HTTP500PrecursorException -> IO ()
     --        )

    sessionPollThread session_data headers_output


clientWorkerThread :: GlobalStreamId -> Effect -> MVar HeadersSent -> InputDataStream -> WorkerMonad ()
clientWorkerThread stream_id effects headers_sent input_data_stream = do
    -- And now work on sending the data, if any...
    _ <- runConduit $
        transPipe liftIO input_data_stream
        `fuseBothMaybe`
        sendDataOfStream stream_id headers_sent effects
    return ()


clientSideTerminate  :: ClientState -> ConnectionCloseReason  -> IO ()
clientSideTerminate client_state reason = do
    let
        terminateOnQueue :: ConnectionCloseReason -> MVar (Message, MVar RequestResult) -> IO ()
        terminateOnQueue reason' q = do
            w <- tryTakeMVar q
            case w of
                Just (_, m) -> do
                    _ <- tryPutMVar m (Left reason')
                    return ()

                Nothing ->
                    return ()


        terminate :: ConnectionCloseReason -> IO ()
        terminate reason' = do
            terminateOnQueue reason' (client_state ^. pendingRequests_ClS)

            -- Kill any waiters in the map
            H.mapM_
                (\(_k,v) -> tryPutMVar v (Left reason))
                (client_state ^. response2Waiter_ClS)

        client_is_closed_mvar = client_state ^. clientIsClosed_ClS

    -- Let's also mark the session as closed from the client side, so that
    -- any further requests end with the correct exception
    modifyMVar_ client_is_closed_mvar $ \ client_is_closed -> do
            unless client_is_closed $
                terminate reason
            return True
