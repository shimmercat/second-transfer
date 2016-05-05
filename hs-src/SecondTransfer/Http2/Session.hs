-- Session: links frames to streams, and helps in ordering the header frames
-- so that they don't get mixed with header frames from other streams when
-- resources are being served concurrently.
{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings, BangPatterns #-}
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
    ,SessionOutput
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
import qualified Control.Concurrent.BoundedChan         as BC
import           Control.Exception                      (throwTo)
import qualified Control.Exception                      as E
import           Control.Monad                          (
                                                         forever,
                                                         unless,
                                                         when,
--                                                         mapM_,
                                                         forM,
                                                         forM_)
import           Control.Monad.Morph                    (hoist, lift)
import           Control.Monad.IO.Class                 (liftIO, MonadIO)
import           Control.DeepSeq                        (
                                                         --($!!),
                                                        deepseq )
import           Control.Monad.Trans.Reader
--import           Control.Monad.Trans.Class              (lift)
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

import qualified Control.Monad.Trans.Resource           as ReT
import           System.Clock                            ( getTime
                                                         , Clock(..)
                                                         , toNanoSecs
                                                         , diffTimeSpec
                                                         , TimeSpec
                                                         )


-- Imports from other parts of the program
import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.MainLoop.Tokens
--import           SecondTransfer.MainLoop.Protocol
import           SecondTransfer.MainLoop.ClientPetitioner

import           SecondTransfer.Sessions.Config
import           SecondTransfer.IOCallbacks.Types       (ConnectionData, addr_CnD)
import           SecondTransfer.Sessions.Internal       (--sessionExceptionHandler,
                                                         SessionsContext,
                                                         sessionsConfig)
import           SecondTransfer.Utils                   (unfoldChannelAndSource, bs8BEtoWord64)
import           SecondTransfer.Exception
import qualified SecondTransfer.Utils.HTTPHeaders       as He
import qualified SecondTransfer.Http2.TransferTypes     as TT
#ifdef SECONDTRANSFER_MONITORING
import           SecondTransfer.MainLoop.Logging        (logit)

#endif

import           Debug.Trace                            (traceShowId)


type InputFrame  = NH2.Frame


--useChunkLength :: Int
-- useChunkLength = 2048


-- What to do regarding headers
data HeaderOutputMessage =
    -- Send the headers of the principal stream
    NormalResponse_HM (GlobalStreamId,  Headers, Effect, MVar TT.OutputDataFeed)
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
    _streamId_WTE                 :: GlobalStreamId

    -- For high priority headers information
    , _headersOutput_WTE          :: Chan HeaderOutputMessage

    -- And regular contents can come this way and thus be properly mixed
    -- with everything else.... for now...
    ,_streamBytesSink_WTE         :: MVar TT.OutputDataFeed

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

newtype SessionOutputChannelAbstraction = SOCA (BC.BoundedChan TT.SessionOutputPacket)

sendOutputToFramer :: SessionOutputChannelAbstraction -> TT.SessionOutputPacket -> IO ()
sendOutputToFramer (SOCA chan) p = {-# SCC serial #-}  p `seq` BC.writeChan chan p

newSessionOutput :: IO SessionOutputChannelAbstraction
newSessionOutput =
  do
    -- It could be length 1, but 8 will give it a bit more of space...
    chan <- BC.newBoundedChan 8
    return . SOCA $ chan

-- From outside, one can only read from this one
type SessionOutput = SessionOutputChannelAbstraction
getFrameFromSession :: SessionOutput -> IO TT.SessionOutputPacket
getFrameFromSession (SOCA chan) = BC.readChan chan


type HashTable k v = H.CuckooHashTable k v


type Stream2HeaderBlockFragment = HashTable GlobalStreamId Bu.Builder


type WorkerMonad = ReaderT WorkerThreadEnvironment IO


-- |Have to figure out which are these...but I would expect to have things
-- like unexpected aborts here in this type.
data SessionInputCommand =
    FirstFrame_SIC InputFrame               -- | This frame is special
    |MiddleFrame_SIC InputFrame             -- | Ordinary frame
    |InternalAbort_SIC                      -- | Internal abort from the session itself
    |InternalAbortStream_SIC GlobalStreamId  -- | Internal abort, but only for a frame
    |CancelSession_SIC                      -- |Cancel request from the framer
    |PingFrameEmitted_SIC (Int, TimeSpec)   -- |The Framer decided to emit a ping request, this is the sequence number (of the packet it was sent on) and the time
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
    --
    --  TODO: This outer MVar is no longer needed, since multi-headers and
    --  and such are handled specially now . REMOVE!
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

    -- Used for decoding the headers... actually, this dictionary should
    -- even contain just one entry... BIG TODO!!!
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
    -- Notice that ThreadIds here retain the thread's stack in place.
    ,_stream2WorkerThread        :: HashTable Int ThreadId

    -- Use to retrieve/set the session id
    ,_sessionIdAtSession         :: ! Int

    -- And used to keep peer session settings
    ,_sessionSettings            :: SessionSettings

    -- What is the next stream available for push?
    ,_nextPushStream             :: MVar Int

    -- What role does this session has?
    ,_sessionRole                :: SessionRole

    -- When did we start this session?
    ,_startTime                  :: TimeSpec

    -- The address of the peer.
    ,_peerAddress                :: Maybe HashableSockAddr

    -- Used to decide what to do when some exceptions bubble
    ,_sessionIsEnding            :: DIO.IORef Bool

    -- Used to store latency reports
    ,_latencyReports             :: MVar [(Int, Double)]

    -- Used to store information about an emitted ping
    -- frame in the connection
    ,_emittedPings               :: MVar [(Int, TimeSpec)]
    }


makeLenses ''SessionData


instance ActivityMeteredSession SessionData where
    sessionLastActivity s = return $ s ^. startTime

instance CleanlyPrunableSession SessionData where
    cleanlyCloseSession s = runReaderT (quietlyCloseConnection NH2.NoError) s


http2ServerSession :: ConnectionData -> AwareWorker -> Int -> SessionsContext -> IO Session
http2ServerSession conn_data a i sctx = http2Session (Just conn_data) Server_SR a (error "NotAClient") i sctx


http2ClientSession :: ClientState -> Int -> SessionsContext -> IO Session
http2ClientSession client_state session_id sctx =
    http2Session Nothing Client_SR (error "NotAServer") client_state session_id sctx


--                                v- {headers table size comes here!!}
http2Session :: Maybe ConnectionData -> SessionRole -> AwareWorker -> ClientState  -> Int -> SessionsContext -> IO Session
http2Session maybe_connection_data session_role aware_worker client_state session_id sessions_context =   do
    session_input             <- newChan
    session_output            <- newSessionOutput
    session_output_mvar       <- newMVar session_output


    -- For incremental construction of headers...
    stream_request_headers    <- H.new :: IO Stream2HeaderBlockFragment

    -- Warning: we should find a way of coping with different table sizes.
    decode_headers_table      <- HP.newDynamicTableForDecoding 4096 8192
    decode_headers_table_mvar <- newMVar decode_headers_table

    encode_headers_table      <- HP.newDynamicTableForEncoding 4096
    encode_headers_table_mvar <- newMVar encode_headers_table

    -- These ones need independent threads taking care of sending stuff
    -- their way...
    headers_output            <- newChan :: IO (Chan HeaderOutputMessage)

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

    start_time                <- getTime Monotonic
    session_is_ending_ioref   <- DIO.newIORef False

    -- What about stream cancellation?
    cancelled_streams_mvar    <- newMVar $ NS.empty :: IO (MVar NS.IntSet)

    -- Empty initial latency report
    latency_reports           <- newMVar []
    emitted_pings             <- newMVar []

    let
        for_worker_thread = WorkerThreadEnvironment {
             _streamId_WTE = error "NotInitialized"
          ,  _headersOutput_WTE = headers_output
          ,  _streamsCancelled_WTE = cancelled_streams_mvar
          ,  _sessionSettings_WTE = session_settings
          ,  _nextPushStream_WTE = next_push_stream
          ,  _resetStreamButton_WTE = error "Not initialized"
          ,  _childResetStreamButton_WTE = error "Not initialized"
          ,  _streamBytesSink_WTE = error "Not initialized"
        }

        maybe_hashable_addr = case maybe_connection_data of
            Just connection_info ->  connection_info ^. addr_CnD
            Nothing -> Nothing

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
        ,_startTime                  = start_time
        ,_peerAddress                = maybe_hashable_addr
        ,_sessionIsEnding            = session_is_ending_ioref
        ,_latencyReports             = latency_reports
        ,_emittedPings               = emitted_pings
        }

    let

        new_session :: HashableSockAddr -> SessionGenericHandle -> forall a . a -> IO ()
        new_session a b c = case maybe_callback of
            Just (NewSessionCallback callback) -> callback a b c
            Nothing -> return ()
          where
            maybe_callback =
                (sessions_context ^. (sessionsConfig . sessionsCallbacks . newSessionCallback_SC) )

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
        exc_handler _component e = do
            case session_role of
                Server_SR ->
                    -- May be leaking space here, disabling for now since we are not using it.
                    -- sessionExceptionHandler component session_id sessions_context e
                    return ()

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

    -- New session! TODO: Have to fix this manager code maybe?
    case maybe_hashable_addr of
          Just hashable_addr ->
              new_session
                 hashable_addr
                 (Whole_SGH session_data)
                 session_data

          Nothing ->
              return ()


    -- If I'm a client, I also need a thread to poll for requests
    when (session_role == Client_SR) $ do
         _ <- forkIOExc "s2f4" $ exc_guard SessionClientPollThread_HTTP2SessionComponent
                $ sessionPollThread session_data headers_output
         return ()


    -- The two previous threads fill the session_output argument below (they write to it)
    -- the session machinery in the other end is in charge of sending that data through the
    -- socket.

    return ( (SessionInput session_input),
             session_output )

-- | Takes frames from the Framer and starts streams mini-workers to make sense
-- of them.
--
-- TODO: Really limit the number of streams that a single client is allowed to have active.
--
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
    -- stream2workerthread       <- view stream2WorkerThread
    -- receiving_headers_mvar    <- view receivingHeaders
    -- last_good_stream_mvar     <- view lastGoodStream
    -- current_session_id        <- view sessionIdAtSession

    input                     <- {-# SCC session_input #-} liftIO $ readChan session_input
    session_role              <- view sessionRole

    case input of

        FirstFrame_SIC
            (NH2.Frame
                (NH2.FrameHeader _ 0 null_stream_id )
                (NH2.SettingsFrame settings_list)
            ) | 0 == null_stream_id  -> do
            -- Good, handle
            handleSettingsFrame settings_list
            continue

        FirstFrame_SIC (NH2.Frame
            (NH2.FrameHeader _ 1 null_stream_id )  (NH2.SettingsFrame _ ) ) |  0 == null_stream_id  -> do
            -- This is a SETTINGS ACK frame, which is okej to have,
            -- do nothing here
            continue

        FirstFrame_SIC _ -> do
            -- Bad, incorrect id or god knows only what ....
            closeConnectionBecauseIsInvalid NH2.ProtocolError
            return ()

        PingFrameEmitted_SIC x -> do
            -- Used by the Forwarded infra-structure to assess the latency to
            -- the client.
            emitted_pings_mvar <- view emittedPings
            liftIO . modifyMVar_ emitted_pings_mvar $ \ emitted_pings ->
                return $ x : emitted_pings
            continue

        CancelSession_SIC -> do
            -- Good place to tear down worker threads... Let the rest of the finalization
            -- to the framer.
            --
            -- This message is normally got from the Framer
            --
            cancellAllStreams

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
            is_iddle <- streamIsIdle stream_id
            if ( stream_id == 0 || (is_iddle && odd stream_id ) )
              then do
                closeConnectionBecauseIsInvalid NH2.ProtocolError
                return ()
              else do
                reportSituation (StreamResetReceived_SWC stream_id)
                liftIO $ do
                    cancelled_streams <- takeMVar cancelled_streams_mvar
                    putMVar cancelled_streams_mvar $ NS.insert  stream_id cancelled_streams
                closePostDataSource stream_id
                continue

        MiddleFrame_SIC _frame@(NH2.Frame frame_header (NH2.WindowUpdateFrame _credit) ) -> do
             -- The Framer is the one using this information, here I just merely inspect the length and destroy
             -- the session if that length is not good
             let frame_length = frameLength frame_header
             if frame_length /= 4
               then do
                 reportSituation $ PeerErrored_SWC "WindowUpdateFrameIncorrectSize"
                 closeConnectionBecauseIsInvalid NH2.FrameSizeError
                 return ()
               else
                 continue

        MiddleFrame_SIC frame@(NH2.Frame (NH2.FrameHeader _ _ nh2_stream_id) (NH2.DataFrame somebytes)) -> unlessReceivingHeaders $ do
            -- So I got data to process
            -- TODO: Handle end of stream
            let stream_id = nh2_stream_id

            -- The call below will block if there is not space in the mvar which is sending data to the
            was_ok <- streamWorkerSendData stream_id somebytes

            if was_ok
              then do
                -- After that data has been received and forwarded downstream, we can issue a windows update
                --
                --
                -- TODO: I think we are sending data the correct way, but there are no warranties

                if frameEndsStream frame
                  then do
                    -- Good place to close the source ...
                    closePostDataSource stream_id
                    when (B.length somebytes > 0) $ do
                        sendOutPriorityTrainMany [
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
                  else
                    when (B.length somebytes > 0) $ do
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
                continue

              else do
                -- For some reason there is no PostInput processing mechanism, therefore,
                -- we were not expecting data at this point
                reportSituation (PeerErrored_SWC "DataReceivedOnUnreadyStream")
                closeConnectionBecauseIsInvalid NH2.ProtocolError
                return ()

        MiddleFrame_SIC (NH2.Frame frame_header (NH2.PingFrame _)) | not (isStreamZero frame_header)  || frameLength frame_header /= 8  -> do
            reportSituation (PeerErrored_SWC "PingFrameWithStreamIdNotZero")
            closeConnectionBecauseIsInvalid NH2.ProtocolError
            return ()

        MiddleFrame_SIC (NH2.Frame (NH2.FrameHeader _ flags _) (NH2.PingFrame ping_payload)) | NH2.testAck flags-> do
            -- Deal with pings: this is an Ack, register it if possible...
            let
                seq_no = fromIntegral $ bs8BEtoWord64 ping_payload
            emitted_pings_mvar <- view emittedPings
            latency_report_mvar <- view latencyReports
            liftIO $ do
                now <- getTime Monotonic
                -- putStrLn $ "Received ack for " ++ (show seq_no)
                modifyMVar_ emitted_pings_mvar $ \ emitted_pings ->
                    modifyMVar latency_report_mvar $ \ latency_report -> do
                        let
                            lookupX ((sn, ws):rest) | sn == seq_no = (Just ws, rest)
                                                    | otherwise =
                                                       let
                                                           (r , n) = lookupX rest
                                                        in (r, (sn, ws):n)
                            lookupX [] = (Nothing, [])
                            (maybe_when_sent, new_emitted_pings) =  lookupX $
                                emitted_pings
                        case maybe_when_sent of
                            Just when_sent
                              | length latency_report < 8 -> do
                                let
                                    tspec = now `diffTimeSpec` when_sent
                                    milliseconds = (fromIntegral $
                                        toNanoSecs tspec ) / 1.0e6
                                    new_latency_report = latency_report ++ [(seq_no, milliseconds)]
                                return (new_latency_report, new_emitted_pings)
                              | otherwise -> do
                                -- Just remote the old ping
                                return (latency_report, new_emitted_pings)

                            _ -> return (latency_report, emitted_pings)
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

        MiddleFrame_SIC (NH2.Frame frame_header (NH2.SettingsFrame settings_list))
          | frameLength frame_header `mod` 6 /= 0 -> do
            reportSituation (PeerErrored_SWC "SettingsFrameSizeError")
            closeConnectionBecauseIsInvalid NH2.FrameSizeError
            return ()

          | isSettingsAck frame_header && isStreamZero frame_header && isLengthZero frame_header  -> do
            -- Frame was received by the peer, do nothing here...
            continue

          | not (isSettingsAck frame_header) && isStreamZero frame_header -> do
            handleSettingsFrame settings_list
            continue

          | otherwise  -> do
            -- Frame was received by the peer, do nothing here...
            reportSituation (PeerErrored_SWC "SettingsFrameSizeError")
            closeConnectionBecauseIsInvalid NH2.ProtocolError
            return ()

        MiddleFrame_SIC (NH2.Frame _ (NH2.GoAwayFrame _ _err _ ))
            | Server_SR <- session_role -> do
                -- I was sent a go away, so go-away...
                --liftIO . putStrLn $ "Received GoAway frame"
                reportSituation $ConnectionCloseReceived_SWC
                quietlyCloseConnection NH2.NoError
                return ()

            | Client_SR <- session_role -> do
                -- I was sent a go away, so go-away, but use the kind of exception
                -- that will unwind the stack gracefully
                _ <- closeConnectionForClient NH2.NoError
                return ()

        MiddleFrame_SIC (NH2.Frame  (NH2.FrameHeader _ _ nh2_stream_id) (NH2.PriorityFrame NH2.Priority {NH2.exclusive=_e, NH2.streamDependency=dep_id, NH2.weight=_w}  ) )
            | nh2_stream_id == dep_id -> do
                reportSituation (PeerErrored_SWC "InvalidPriorityFrame")
                closeConnectionBecauseIsInvalid NH2.ProtocolError
                return ()

            | otherwise ->
                continue

        MiddleFrame_SIC _somethingelse ->  unlessReceivingHeaders $ do
            -- An undhandled case here....
            reportSituation UnknownFrame_SWC
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

            -- Handled by the framer, but errors should be reported here.
            max_flow_control_size = lookup NH2.SettingsInitialWindowSize _settings_list

        ok <- case enable_push of
            Just 1      -> do
                liftIO $ DIO.writeIORef (session_settings ^. pushEnabled_SeS) True
                return True
            Just 0      -> do
                liftIO $ DIO.writeIORef (session_settings ^. pushEnabled_SeS) False
                return True
            Just _      -> do
                closeConnectionBecauseIsInvalid NH2.ProtocolError
                return True

            Nothing     ->  do
                return True

        ok2 <- if ok
                 then
                   case max_frame_size of
                       -- The spec says clearly what's the minimum size that can come here
                       Just n | n < 16384 || n > 16777215
                                   -> do
                                      -- liftIO $ putStrLn "Wild max frame size"
                                      closeConnectionBecauseIsInvalid NH2.FrameSizeError
                                      return False

                              | otherwise
                                   ->
                                      if n > (sessions_config ^. dataFrameSize)
                                         -- Ignore if it is bigger than the size configured in this context
                                         then do
                                            return True
                                         else do
                                             liftIO $  DIO.writeIORef (session_settings ^. frameSize_SeS) n
                                             return True
                       Nothing     -> return True
                 else
                   return False

        ok3 <- if ok2
                 then
                   case max_flow_control_size of
                       Just n
                         | n > 2147483647 || n < 0
                             -> do
                                    closeConnectionBecauseIsInvalid NH2.FlowControlError
                                    return False
                         | otherwise
                            -> return True
                       Nothing -> return True
                 else
                   return False


        if ok3
          then
            sendOutPriorityTrain
                (NH2.EncodeInfo
                    (NH2.setAck NH2.defaultFlags)
                    0
                    Nothing
                )
                (NH2.SettingsFrame [])
          else
            return ()


reportSituation :: SituationWithClient -> ReaderT SessionData IO ()
reportSituation situation =
  do
    session_id <- view sessionIdAtSession
    maybe_situation_callback <- view
        (sessionsContext . sessionsConfig . sessionsCallbacks . situationCallback_SC)
    case maybe_situation_callback of
        Nothing -> return ()
        Just callback -> liftIO $ callback session_id situation


streamIsIdle :: GlobalStreamId -> ReaderT SessionData IO Bool
streamIsIdle stream_id =
  do
    last_good_stream_mvar <- view lastGoodStream
    last_good_stream <- liftIO . readMVar $ last_good_stream_mvar
    return ( stream_id > last_good_stream )


serverProcessIncomingHeaders :: NH2.Frame ->  ReaderT SessionData IO ()
serverProcessIncomingHeaders frame | Just (!stream_id, bytes) <- isAboutHeaders frame = do

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
    maybe_hashable_addr       <- view peerAddress
    session_settings          <- view sessionSettings

    if opens_stream
      then {-# SCC gpAb #-} do
        --maybe_rcv_headers_of <- liftIO $ takeMVar receiving_headers_mvar
        all_ok <- liftIO . modifyMVar receiving_headers_mvar $ \  maybe_rcv_headers_of ->
            case maybe_rcv_headers_of of
                Just _ ->
                    -- Bad peer, it is already sending headers
                    -- and trying to open another one
                    return (maybe_rcv_headers_of, False)
                Nothing -> do
                    return (Just stream_id, True)
        if all_ok
          then do
              -- And go to check if the stream id is valid
              ok2 <- liftIO . modifyMVar last_good_stream_mvar $  \ last_good_stream ->
                  if (odd stream_id ) && (stream_id > last_good_stream)
                    then
                        -- We are golden, set the new good stream
                        return (stream_id, True)
                    else
                        -- The new oppened stream has a new id
                        return (stream_id, False)
              unless ok2 $
                  closeConnectionBecauseIsInvalid NH2.ProtocolError
          else do
            closeConnectionBecauseIsInvalid NH2.ProtocolError
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

    if frameEndsHeaders frame then
      do
        -- Ok, let it be known that we are not receiving more headers
        liftIO $ modifyMVar_
            receiving_headers_mvar
            (\ _ -> return Nothing )
        -- Lets get a time
        headers_arrived_time      <- liftIO $ getTime Monotonic

        -- This is where the bytes of the stream will end up .
        stream_bytes <- liftIO newEmptyMVar

        let
            reset_button  = writeChan session_input (InternalAbortStream_SIC stream_id)
            child_reset_button = \stream_id' -> writeChan session_input  (InternalAbortStream_SIC stream_id')
            -- Prepare the environment for the new working thread
            for_worker_thread     =
                (set streamId_WTE stream_id)
                .
                (set streamBytesSink_WTE stream_bytes)
                .
                (set resetStreamButton_WTE reset_button)
                .
                (set childResetStreamButton_WTE child_reset_button)
                $
                for_worker_thread_uns

        -- Let's decode the headers
        headers_bytes             <- getHeaderBytes stream_id
        either_header_list <- liftIO . withMVar decode_headers_table_mvar $ \ dyn_table -> do
            E.catch
                (do
                   r  <- HP.decodeHeader dyn_table headers_bytes
                   return . Right $ r )
                ((const $ return $ Left () ):: HP.DecodeError -> IO (Either ()  HP.HeaderList))

        case either_header_list of
            Left _ -> do
                reportSituation (PeerErrored_SWC "InvalidHeaders")
                closeConnectionBecauseIsInvalid NH2.ProtocolError

            Right (header_list ) -> do
                -- Good moment to remove the headers from the table.... we don't want a space
                -- leak here
                liftIO $ do
                    H.delete stream_request_headers stream_id

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

                -- We are going to read this here because we want to
                -- pass it to the worker a layer up.
                push_enabled <-
                    liftIO . DIO.readIORef $
                        session_settings ^. pushEnabled_SeS

                latency_report_mvar <-
                    view latencyReports
                latency_report <-
                    liftIO . readMVar $ latency_report_mvar


                let
                    perception = Perception {
                        _startedTime_Pr = headers_arrived_time,
                        _streamId_Pr = stream_id,
                        _sessionId_Pr = current_session_id,
                        _protocol_Pr = Http2_HPV,
                        _anouncedProtocols_Pr = Nothing,
                        _peerAddress_Pr = maybe_hashable_addr,
                        _pushIsEnabled_Pr = push_enabled,
                        _sessionLatencyRegister_Pr = latency_report
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

                session_is_ending_ioref <- view sessionIsEnding
                liftIO $ do
                    -- The mvar below: avoid starting until the entry has
                    -- been properly inserted in the table...
                    ready <- newMVar ()
                    let
                        general_exc_handler :: E.SomeException -> IO ()
                        general_exc_handler e = do
                            -- Actions to take when the thread breaks....
                            -- We cancel the entire session because there is a more specific
                            -- handler that doesn't somewhere below. If the exception bubles here,
                            -- it is because the situation is out of control. We may as well
                            -- exit the server, but I'm not being so extreme now.
                            H.delete stream2workerthread stream_id
                            session_is_ending <- DIO.readIORef session_is_ending_ioref
                            unless session_is_ending $ do
                                putStrLn $ "ERROR: Aborting session after non-handled exception bubbled up " ++ E.displayException e
                                writeChan session_input InternalAbort_SIC
                        io_closed_handle :: E.BlockedIndefinitelyOnMVar -> IO ()
                        io_closed_handle _e = return ()
                    thread_id <-
                      forkIOExc "s2f7" .
                      E.handle general_exc_handler .
                      E.handle io_closed_handle $
                          ({-# SCC growP1 #-} do
                              putMVar ready ()
                              runReaderT
                                  (workerThread
                                         request'
                                         coherent_worker)
                                  for_worker_thread
                              H.delete stream2workerthread stream_id
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
        header_list <- liftIO . withMVar decode_headers_table_mvar $ \ dyn_table ->
            HP.decodeHeader dyn_table headers_bytes

        -- Good moment to remove the headers from the table.... we don't want a space
        -- leak here
        liftIO $ do
            H.delete stream_request_headers stream_id

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

    liftIO $ withMVar session_output_mvar $ \ session_output ->
      sendOutputToFramer session_output  $ TT.PriorityTrain_StFB [(
          encode_info,
          payload,
          -- Not sending effects in this frame, since it is not related...
          error "sendOutFrameNotFor")]


sendOutPriorityTrainMany :: [(NH2.EncodeInfo , NH2.FramePayload)]  -> ReaderT SessionData IO ()
sendOutPriorityTrainMany many = do
    session_output_mvar <- view sessionOutput

    liftIO $ withMVar session_output_mvar $ \ session_output ->
        sendOutputToFramer session_output $
            TT.PriorityTrain_StFB $
            map
                (\(encode_info, payload) -> (encode_info, payload, error "no-effect"))
                many


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
    last_good_stream_mvar <- view lastGoodStream
    last_good_stream <- liftIO $ takeMVar last_good_stream_mvar

    requestTermination last_good_stream error_code
    cancellAllStreams


-- Sends a GO_AWAY frame and closes everything, without being too drastic
quietlyCloseConnection :: NH2.ErrorCodeId -> ReaderT SessionData IO ()
quietlyCloseConnection error_code = do
    -- liftIO $ putStrLn "quietlyCloseConnection"
    last_good_stream_mvar <- view lastGoodStream
    last_good_stream <- liftIO $ takeMVar last_good_stream_mvar

    cancellAllStreams
    requestTermination last_good_stream error_code


-- Sends a GO_AWAY frame and raises an exception, effectively terminating the input
-- thread of the session. This one for the client is different because it throws
-- an exception of type ClientSessionAbortedException
closeConnectionForClient :: NH2.ErrorCodeId -> ReaderT SessionData IO a
closeConnectionForClient error_code = do
    let
        use_reason = case error_code of
            NH2.NoError -> NormalTermination_CCR
            _           -> ProtocolError_CCR

    last_good_stream_mvar <- view lastGoodStream
    last_good_stream <- liftIO $ takeMVar last_good_stream_mvar

    client_is_closed_mvar <- view (simpleClient . clientIsClosed_ClS )
    requestTermination last_good_stream error_code
    cancellAllStreams
    liftIO $ do

        -- Let's also mark the session as closed from the client side, so that
        -- any further requests end with the correct exception
        modifyMVar_ client_is_closed_mvar (\ _ -> return True)

        -- And unwind the input thread in the session, so that the
        -- exception handler runs....
        E.throw $ ClientSessionAbortedException use_reason


cancellAllStreams :: ReaderT SessionData IO ()
cancellAllStreams =
  do
    session_is_ending_ioref <- view sessionIsEnding
    stream2workerthread <- view stream2WorkerThread

    liftIO $ do
        DIO.writeIORef session_is_ending_ioref True
        -- Close all active threads for this session
        H.mapM_
            ( \(_stream_id, thread_id) ->
                    throwTo thread_id StreamCancelledException
            )
            stream2workerthread


requestTermination :: GlobalStreamId -> NH2.ErrorCodeId -> ReaderT SessionData IO ()
requestTermination stream_id error_code =
  do
    session_is_ending_ioref <- view sessionIsEnding
    liftIO $ DIO.writeIORef session_is_ending_ioref True
    session_output_mvar <- view sessionOutput
    let
        message = TT.Command_StFB (TT.SpecificTerminate_SOC stream_id error_code)
    liftIO . withMVar session_output_mvar $ \ session_output -> do
        sendOutputToFramer session_output message


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
    let pim = PostInputMechanism (chan, hoist lift  source)
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


isLengthZero :: NH2.FrameHeader -> Bool
isLengthZero (NH2.FrameHeader l _ _ ) = l == 0

frameLength :: NH2.FrameHeader -> Int
frameLength (NH2.FrameHeader l _ _ ) = l

isStreamZero :: NH2.FrameHeader -> Bool
isStreamZero (NH2.FrameHeader _  _ s) = s == 0


isStreamCancelled :: GlobalStreamId  -> WorkerMonad Bool
isStreamCancelled stream_id = do
    cancelled_streams_mvar <- view streamsCancelled_WTE
    cancelled_streams <- liftIO $ readMVar cancelled_streams_mvar
    return $ NS.member stream_id cancelled_streams


markStreamCancelled :: GlobalStreamId  -> WorkerMonad ()
markStreamCancelled stream_id = do
    cancelled_streams_mvar <- view streamsCancelled_WTE
    liftIO . modifyMVar_ cancelled_streams_mvar $ \ cancelled_streams ->
        return $ NS.insert  stream_id cancelled_streams
    return ()


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


-- | Invokes the Coherent worker, and interacts with the rest of
--   the session and the Framer so that data is sent and received by
--   the peer following the protocol.
workerThread :: Request -> AwareWorker -> WorkerMonad ()
workerThread req aware_worker =
    ignoreCancels $  do
        headers_output <- view headersOutput_WTE
        stream_id      <- view streamId_WTE
        --session_settings <- view sessionSettings_WTE
        --next_push_stream_mvar <-  view nextPushStream_WTE

        -- If the request get rejected right away, we can just send
        -- a 500 error in this very stream, without making any fuss.
        -- (headers, _, data_and_conclussion)
        --
        -- TODO: Can we add debug information in a header here?
        principal_stream <-
            liftIO $ {-# SCC wTer1  #-} E.catch
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
                {-# SCC nHS #-} normallyHandleStream principal_stream

            Just (InterruptConnectionAfter_IEf) -> do
                normallyHandleStream principal_stream
                liftIO . writeChan headers_output $ GoAway_HM (stream_id, effects)

            Just (InterruptConnectionNow_IEf) -> do
                -- Not one hundred-percent sure of this being correct, but we don't want
                -- to acknowledge reception of this stream then
                let use_stream_id = stream_id - 1
                liftIO . writeChan headers_output $ GoAway_HM (use_stream_id, effects)
  where
    ignoreCancels = CMC.handle ((\_ -> return () ):: StreamCancelledException -> WorkerMonad ())


normallyHandleStream :: PrincipalStream -> WorkerMonad ()
normallyHandleStream principal_stream = do
    headers_output <- view headersOutput_WTE
    stream_id      <- view streamId_WTE
    session_settings <- view sessionSettings_WTE
    next_push_stream_mvar <-  view nextPushStream_WTE
    reset_button <- view resetStreamButton_WTE
    this_environment <- ask

    -- Pieces of the header
    let
       headers              = He.removeConnectionHeaders $ principal_stream ^. headers_PS
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
                -- We do not expect connection headers in pushed streams, since they
                -- are HTTP/2-specific.
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
    data_output <- view streamBytesSink_WTE
    headers `seq` ( liftIO $ writeChan headers_output $ NormalResponse_HM (stream_id, headers, effects, data_output) )

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
        let
            check_if_cancelled =
                runReaderT
                    (isStreamCancelled stream_id)
                    this_environment
        _ <- liftIO . ReT.runResourceT $ do
            resource_key <- ReT.register reset_button
            _ <- runConduit $
               (data_and_conclusion)
               `fuseBothMaybe`
               (sendDataOfStream check_if_cancelled data_output)
            ReT.unprotect resource_key

        -- BIG TODO: Send the footers ... likely stream conclusion semantics
        -- will need to be changed.

        -- AFTER sending the data of the main stream, start sending the data of the
        -- pushed streams...

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
                  liftIO .
                      forkIOExc "s2f8" .
                      ignoreException blockedIndefinitelyOnMVar () $
                      runReaderT action environment

        return ()



-- Takes care of pushed data, which  is sent through pipes to
-- the output thread here in this session.
pusherThread :: GlobalStreamId -> Headers -> DataAndConclusion -> Effect  -> WorkerMonad ()
pusherThread child_stream_id response_headers pushed_data_and_conclusion effects =
  do
    headers_output <- view headersOutput_WTE
    -- session_settings <- view sessionSettings_WTE
    pushed_reset_button <- view childResetStreamButton_WTE

    -- TODO: Handle exceptions here: what happens if the coherent worker
    --       throws an exception signaling that the request is ill-formed
    --       and should be dropped? That could happen in a couple of occassions,
    --       but really most cases should be handled here in this file...
    -- (headers, _, data_and_conclussion)
    pusher_data_output <- liftIO $ newEmptyMVar

    -- Now I send the headers, if that's possible at all. These are classes as "Normal response"
    liftIO . writeChan headers_output
        $ NormalResponse_HM (child_stream_id,  response_headers, effects, pusher_data_output)

    this_environment <- ask

    let
        check_if_cancelled =
            runReaderT
                (isStreamCancelled child_stream_id)
                this_environment

    -- At this moment I should ask if the stream hasn't been cancelled by the browser before
    -- commiting to the work of sending addtitional data... this is important for pushed
    -- streams
    is_stream_cancelled <- isStreamCancelled child_stream_id
    unless  is_stream_cancelled $ do
        _ <- liftIO . ReT.runResourceT $ do
            k <- ReT.register (pushed_reset_button child_stream_id)
            _ <- runConduit $
                 pushed_data_and_conclusion
                `fuseBothMaybe`
                sendDataOfStream check_if_cancelled pusher_data_output
            ReT.unprotect k

        return ()


--                                                       v-- comp. monad.
sendDataOfStream :: MonadIO m => (IO Bool) -> MVar TT.OutputDataFeed  -> Sink B.ByteString m ()
sendDataOfStream  check_if_cancelled data_output  =
  do
    consumer
  where
    consumer  = do
        maybe_bytes <- await
        is_stream_cancelled <- liftIO check_if_cancelled
        unless is_stream_cancelled $ do
            case maybe_bytes of

                Nothing -> do
                    -- This is how we finish sending data
                    liftIO $ putMVar data_output  ""

                Just bytes
                    | lng <- B.length bytes, lng > 0   -> do

                        liftIO $ do
                            putMVar data_output bytes
                        consumer

                    | otherwise -> do
                        -- Finish sending data, finish in general
                        liftIO $ putMVar data_output ""


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
        NormalResponse_HM (stream_id, headers, effect, data_output)  -> do

            -- First encode the headers using the table
            encode_dyn_table_mvar <- view toEncodeHeaders

            -- encode_dyn_table <- liftIO $ takeMVar encode_dyn_table_mvar
            data_to_send  <- liftIO . withMVar encode_dyn_table_mvar $ \ encode_dyn_table ->
                HP.encodeHeader HP.defaultEncodeStrategy 8192 encode_dyn_table headers

            -- Now split the bytestring in chunks of the needed size....
            -- Note that the only way we can
            let
                bs_chunks = bytestringChunk use_chunk_length  data_to_send

            -- And send the chunks through while locking the output place....
            liftIO $ bs_chunks `deepseq` withMVar session_output_mvar $ \ session_output -> do
                    let
                        header_frames = headerFrames stream_id bs_chunks effect
                    sendOutputToFramer session_output
                        $ TT.HeadersTrain_StFB (stream_id, header_frames, effect, data_output)

        GoAway_HM (stream_id, _effect) -> do
           -- This is in charge of sending an interrupt message to the framer
           requestTermination stream_id NH2.NoError

        PushPromise_HM (parent_stream_id, child_stream_id, promise_headers, effect) -> do

            encode_dyn_table_mvar <- view toEncodeHeaders

            -- encode_dyn_table <- liftIO $ takeMVar encode_dyn_table_mvar

            data_to_send  <- liftIO . withMVar encode_dyn_table_mvar $ \ encode_dyn_table ->
                HP.encodeHeader HP.defaultEncodeStrategy 8192 encode_dyn_table promise_headers

            -- Now split the bytestring in chunks of the needed size....
            bs_chunks <- return $! bytestringChunk use_chunk_length data_to_send

            -- And send the chunks through while locking the output place....
            liftIO $ withMVar session_output_mvar $ \ session_output ->  do
                    let
                        pp_frames = pushPromiseFrames parent_stream_id child_stream_id bs_chunks effect
                    sendOutputToFramer session_output $ TT.PriorityTrain_StFB pp_frames

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
        worker_environment =
            set
               streamId_WTE
               new_stream_id
               for_worker_thread
        effects = defaultEffects

    -- Store the response place
    new_stream_id `seq` H.insert response2waiter new_stream_id response_mvar


    -- NON-tested code
    output_mvar <- newEmptyMVar

    -- Start by sending the headers of the request
    liftIO $ writeChan headers_output $ NormalResponse_HM (new_stream_id, headers, effects, output_mvar)

    _ <- liftIO . forkIOExc "s2f6" $ do
        either_e0 <- E.try $ runReaderT
            (clientWorkerThread new_stream_id output_mvar input_data_stream )
            worker_environment
        case either_e0 :: Either HTTP500PrecursorException () of
            Left _ -> writeChan session_input $ InternalAbortStream_SIC new_stream_id
            Right _ -> return ()

    sessionPollThread session_data headers_output


clientWorkerThread :: GlobalStreamId  -> MVar TT.OutputDataFeed -> InputDataStream -> WorkerMonad ()
clientWorkerThread stream_id  output_mvar input_data_stream = do
    -- And now work on sending the data, if any...
    this_environment <- ask
    let
        check_if_cancelled =
            runReaderT
                (isStreamCancelled stream_id)
                this_environment
    _ <- liftIO . ReT.runResourceT . runConduit $
        input_data_stream
        `fuseBothMaybe`
        sendDataOfStream check_if_cancelled output_mvar
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
