
-- The framer has two functions: to convert bytes to Frames and the other way around,
-- and two keep track of flow-control quotas.
{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances,
             DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http2.Framer (
    BadPrefaceException,
    SessionPayload(..),

    wrapSession,
    http2FrameLength,

    -- Not needed anywhere, but supress the warning about unneeded symbol
    closeAction
    ) where



import           Control.Concurrent                     hiding (yield)
import qualified Control.Concurrent                     as CC(yield)
import qualified Control.Concurrent.MSem                as MS
import           Control.Concurrent.STM.TMVar
import qualified Control.Concurrent.STM                 as STM
import           Control.Exception
import qualified Control.Exception                      as E
import           Control.Lens                           (view, (^.) )
import qualified Control.Lens                           as L
import           Control.Monad                          (unless, when, replicateM_)
import           Control.Monad.IO.Class                 (liftIO)
import qualified Control.Monad.Catch                    as C
import           Control.Monad.Trans.Class              (lift)
import           Control.DeepSeq                        (($!!))
import           Control.Monad.Trans.Reader
import           Data.Binary                            (decode)
import qualified Data.ByteString                        as B
import           Data.ByteString.Char8                  (pack)
import qualified Data.ByteString.Lazy                   as LB
import qualified Data.ByteString.Builder                as Bu
import           Data.Conduit
import           Data.Foldable                          (find)
import qualified Data.PQueue.Min                        as PQ
import           Data.Maybe                             (fromMaybe)

import qualified Network.HTTP2                          as NH2
-- Logging utilities
import           System.Log.Logger

import qualified Data.HashTable.IO                      as H
import           System.Clock                           (Clock(..),getTime)
-- import           System.Mem.Weak

import           SecondTransfer.Sessions.Internal       (
                                                         sessionExceptionHandler,
                                                         nextSessionId,
                                                         sessionsConfig,
                                                         SessionsContext)
import           SecondTransfer.Sessions.Config
import           SecondTransfer.Http2.Session
import           SecondTransfer.MainLoop.CoherentWorker (AwareWorker, fragmentDeliveryCallback_Ef, priorityEffect_Ef)
import qualified SecondTransfer.MainLoop.Framer         as F
import           SecondTransfer.MainLoop.PushPullType
import           SecondTransfer.Utils                   (Word24, word24ToInt)
import           SecondTransfer.Exception
import           SecondTransfer.MainLoop.Logging        (logWithExclusivity, logit)

import           Debug.Trace                            (trace)

#include "Logging.cpphs"


http2PrefixLength :: Int
http2PrefixLength = B.length NH2.connectionPreface

-- Let's do flow control here here ....

type HashTable k v = H.CuckooHashTable k v


type GlobalStreamId = Int


data FlowControlCommand =
     AddBytes_FCM Int
    |Finish_FCM

-- A hashtable from stream id to channel of availabiliy increases
type Stream2AvailSpace = HashTable GlobalStreamId (MVar FlowControlCommand)


data CanOutput = CanOutput


data NoHeadersInChannel = NoHeadersInChannel


-- Maximum number of packets in the priority queue waiting for
-- delivery. More than this, and I will simply block...
maxPacketsInQueue :: Int
maxPacketsInQueue = 32


-----In this implementation, this  v- and this  v- member should not change during the lieftime
----- of the stream.
-----------------------------------v--priority, v-- stream_id ----|------v-ordinal
newtype PrioPacket = PrioPacket ( (Int ,        Int,                     Int     ),  LB.ByteString)
                     deriving Show

instance Eq PrioPacket where
   (==) (PrioPacket (a,_)) (PrioPacket (b,_)) = a == b

instance Ord PrioPacket where
    compare (PrioPacket (a,_)) (PrioPacket (b,_)) = compare a b


-- Simple thread to prioritize frames in the session
data PrioritySendState = PrioritySendState {
     -- We need a semaphore so that this doesn't get stagnated waiting on writes
     _semToSend :: MS.MSem Int
     ,_prioQ :: TMVar (PQ.MinQueue PrioPacket)
     }


data FramerSessionData = FramerSessionData {

    -- A dictionary (protected by a lock) from stream id to flow control command.
    _stream2flow           :: MVar Stream2AvailSpace

    -- Two members below: the place where you put data which is going to be flow-controlled,
    -- and the place with the ordinal
    -- Flow control dictionary. It goes from stream id to the stream data-output gate (an-mvar)
    -- and to a mutable register for the ordinal. The ordinal for packets inside a stream is used
    -- for priority and reporting
    , _stream2outputBytes    :: MVar ( HashTable GlobalStreamId (MVar LB.ByteString, MVar Int) )

    -- The default flow-control window advertised by the peer (e.g., the browser)
    , _defaultStreamWindow   :: MVar Int

    -- Wait variable to output bytes to the channel. This is needed to avoid output data races.
    , _canOutput             :: MVar CanOutput

    -- Flag that says if the session has been unwound... if such,
    -- threads are adviced to exit as early as possible
    , _outputIsForbidden     :: MVar Bool

    -- Exclusive lock for headers, needed since the specs forbid interleaving header and data
    , _noHeadersInChannel    :: MVar NoHeadersInChannel

    -- The push action
    , _pushAction            :: PushAction

    -- The close action
    , _closeAction           :: CloseAction

    -- Global id of the session, used for e.g. error reporting.
    , _sessionIdAtFramer     :: !Int

    -- Sessions context, used for thing like e.g. error reporting
    , _sessionsContext       :: SessionsContext

    -- For GoAway frames
    -- We keep the value below updated with the highest incoming stream
    -- frame. For example, it is updated as soon as headers are received from
    -- the client on that stream.
    , _lastInputStream       :: MVar Int
    -- We update this one as soon as an outgoing frame is seen with such a
    -- high output number.
    , _lastOutputStream      :: MVar Int

    -- For sending data orderly
    , _prioritySendState     :: PrioritySendState

    -- Need to know this for the preface
    , _sessionRole_FSD       :: SessionRole
    }

L.makeLenses ''FramerSessionData


type FramerSession = ReaderT FramerSessionData IO

data SessionPayload =
    AwareWorker_SP AwareWorker   -- I'm a server
    |ClientState_SP ClientState  -- I'm a client

wrapSession :: SessionPayload -> SessionsContext -> Attendant
wrapSession session_payload sessions_context io_callbacks = do

    let
        session_id_mvar = view nextSessionId sessions_context
        push_action = io_callbacks ^. pushAction_IOC
        pull_action = io_callbacks ^. pullAction_IOC
        close_action = io_callbacks ^. closeAction_IOC
        -- best_effort_pull_action = io_callbacks ^. bestEffortPullAction_IOC


    new_session_id <- modifyMVarMasked
        session_id_mvar $
        \ session_id -> return $ session_id `seq` (session_id + 1, session_id)

    (session_input, session_output) <- case session_payload of
        AwareWorker_SP aware_worker ->  http2ServerSession
                                            aware_worker
                                            new_session_id
                                            sessions_context
        ClientState_SP client_state -> http2ClientSession
                                            client_state
                                            new_session_id
                                            sessions_context

    let
        session_role = case session_payload of
            AwareWorker_SP _ ->  Server_SR
            ClientState_SP _ ->  Client_SR


    -- TODO : Add type annotations....
    s2f                       <- H.new
    stream2flow_mvar          <- newMVar s2f
    s2o                       <- H.new
    stream2output_bytes_mvar  <- newMVar s2o
    default_stream_size_mvar  <- newMVar 65536
    can_output                <- newMVar CanOutput
    no_headers_in_channel     <- newMVar NoHeadersInChannel
    last_stream_id            <- newMVar 0
    last_output_stream_id     <- newMVar 0
    output_is_forbidden       <- newMVar False

    prio_mvar                 <- STM.atomically $ newTMVar PQ.empty
    sem_to_send               <- MS.new maxPacketsInQueue



    -- We need some shared state
    let framer_session_data = FramerSessionData {
        _stream2flow          = stream2flow_mvar
        ,_stream2outputBytes  = stream2output_bytes_mvar
        ,_defaultStreamWindow = default_stream_size_mvar
        ,_canOutput           = can_output
        ,_noHeadersInChannel  = no_headers_in_channel
        ,_pushAction          = push_action
        ,_closeAction         = close_action
        ,_sessionIdAtFramer   = new_session_id
        ,_sessionsContext     = sessions_context
        ,_lastInputStream     = last_stream_id
        ,_lastOutputStream    = last_output_stream_id
        ,_outputIsForbidden   = output_is_forbidden
        ,_prioritySendState   = PrioritySendState {
                                    _semToSend = sem_to_send,
                                    _prioQ = prio_mvar
                                }
        ,_sessionRole_FSD     = session_role
        }


    let
        -- TODO: Dodgy exception handling here...
        close_on_error session_id session_context comp =
            E.finally (
                E.catch
                    (
                        E.catch
                            comp
                            (exc_handler session_id session_context)
                    )
                    (io_exc_handler session_id session_context)
                )
                close_action

        exc_handler :: Int -> SessionsContext -> FramerException -> IO ()
        exc_handler x y e = do
            modifyMVar_ output_is_forbidden (\ _ -> return True)
            -- INSTRUMENTATION( errorM "HTTP2.Framer" "Exception went up" )
            sessionExceptionHandler Framer_HTTP2SessionComponent x y e

        io_exc_handler :: Int -> SessionsContext -> IOProblem -> IO ()
        io_exc_handler _x _y _e =
            modifyMVar_ output_is_forbidden (\ _ -> return True)
            -- !!! These exceptions are way too common for we to care....
            -- errorM "HTTP2.Framer" "Exception went up"
            -- sessionExceptionHandler Framer_HTTP2SessionComponent x y e


    forkIO
        $ close_on_error new_session_id sessions_context
        $ runReaderT (inputGatherer pull_action session_input ) framer_session_data
    forkIO
        $ close_on_error new_session_id sessions_context
        $ runReaderT (outputGatherer session_output ) framer_session_data
    -- Actual data is reordered before being sent
    forkIO
        $ close_on_error new_session_id sessions_context
        $ runReaderT sendReordering framer_session_data

    return ()


http2FrameLength :: F.LengthCallback
http2FrameLength bs
    | B.length bs >= 3     = let
        word24 = decode input_as_lbs :: Word24
        input_as_lbs = LB.fromStrict bs
      in
        Just $ word24ToInt word24 + 9 -- Nine bytes that the frame header always uses
    | otherwise = Nothing


addCapacity ::
        GlobalStreamId ->
        Int           ->
        FramerSession Bool
addCapacity _         0         =
    -- By the specs, a WINDOW_UPDATE with 0 of credit should be considered a protocol
    -- error
    return False
addCapacity 0         delta_cap =
    -- TODO: Implement session flow control
    return True
addCapacity stream_id delta_cap =
    do
        table_mvar <- view stream2flow
        val <- liftIO $ withMVar table_mvar $ \ table ->
            H.lookup table stream_id
        last_stream_mvar <- view lastInputStream
        last_stream <- liftIO . readMVar $ last_stream_mvar
        case val of
            Nothing | stream_id > last_stream ->
                      return False
                    | otherwise -> -- If the stream was seen already, interpret this as a
                                  -- rogue WINDOW_UPDATE and do nothing
                      return True

            Just command_chan -> do
                liftIO $ putMVar command_chan $ AddBytes_FCM delta_cap
                return True


finishFlowControlForStream :: GlobalStreamId -> FramerSession ()
finishFlowControlForStream stream_id =
    do
        table_mvar <- view stream2flow
        liftIO . withMVar table_mvar $ \ table -> do
            val <- H.lookup table stream_id
            case val of
                -- Weird
                Nothing -> return ()

                Just command_chan -> do
                    liftIO $
                        H.delete table stream_id
                    return ()
        table2_mvar <- view stream2outputBytes
        liftIO . withMVar table2_mvar $ \ table -> do
          val <- H.lookup table stream_id
          case val of
              Nothing -> return ()

              Just _ ->
                 liftIO $ H.delete table stream_id


readNextFrame :: Monad m =>
    (Int -> m B.ByteString)                      -- ^ Generator action
    -> Source m (Maybe NH2.Frame)                -- ^ Packet and leftovers, if we could get them
readNextFrame pull_action  = do
    -- First get 9 bytes with the frame header
    frame_header_bs <- lift $ pull_action 9
    -- decode it
    let
        (frame_type_id, frame_header) = NH2.decodeFrameHeader frame_header_bs
        NH2.FrameHeader payload_length _ _ =  frame_header
    -- Get as many bytes as the payload length identifies
    payload_bs <- lift $ pull_action payload_length
    -- Return the entire frame, or raise an exception...
    let
        either_frame = NH2.decodeFramePayload frame_type_id frame_header payload_bs
    case either_frame of
        Right frame_payload -> do
            yield . Just $ NH2.Frame frame_header frame_payload
            readNextFrame pull_action
        Left  _     ->
            yield   Nothing


-- This works by pulling bytes from the input side of the pipeline and converting them to frames.
-- The frames are then put in the SessionInput. In the other end of the SessionInput they can be
-- interpreted according to their HTTP/2 meaning.
--
-- This function also does part of the flow control: it registers WindowUpdate frames and triggers
-- quota updates on the streams.
--
-- Also, when this function exits its caller will issue a close_action
inputGatherer :: PullAction -> SessionInput -> FramerSession ()
inputGatherer pull_action session_input = do

    session_role <- view sessionRole_FSD

    when (session_role == Server_SR) $ do
        -- We can start by reading off the prefix....
        prefix <- liftIO $ pull_action http2PrefixLength
        when (prefix /= NH2.connectionPreface) $ do
            sendGoAwayFrame NH2.ProtocolError
            liftIO $
                -- We just use the GoAway frame, although this is awfully early
                -- and probably wrong
                throwIO BadPrefaceException

    let
        source::Source FramerSession (Maybe NH2.Frame)
        source = transPipe liftIO $ readNextFrame pull_action
    source $$ consume True
  where

    sendToSession :: Bool -> InputFrame -> IO ()
    sendToSession starting frame =
      -- print(NH2.streamId $ NH2.frameHeader frame)
      if starting
        then
          sendFirstFrameToSession session_input frame
        else
          sendMiddleFrameToSession session_input frame

    abortSession :: Sink a FramerSession ()
    abortSession =
      lift $ do
        sendGoAwayFrame NH2.ProtocolError
        -- Inform the session that it can tear down itself
        liftIO $ sendCommandToSession session_input CancelSession_SIC
        -- Any resources remaining here can be disposed
        releaseFramer

    consume_continue = consume False

    consume :: Bool -> Sink (Maybe NH2.Frame) FramerSession ()
    consume starting = do
        maybe_maybe_frame <- await

        -- Consumption ends automatically when the output is forbidden, this might help avoiding
        -- attacks where a peer refuses to close its socket.
        unless output_is_forbidden $ case maybe_maybe_frame of

            Just Nothing      ->
                -- Only way to get here is by a closed connection condition I guess.
                abortSession

            Just (Just right_frame) -> do
                case right_frame of

                    (NH2.Frame (NH2.FrameHeader _ _ stream_id) (NH2.WindowUpdateFrame credit) ) -> do
                        -- Bookkeep the increase on bytes on that stream
                        -- liftIO $ putStrLn $ "Extra capacity for stream " ++ (show stream_id)
                        succeeded <- lift $ addCapacity stream_id (fromIntegral credit)
                        if not succeeded then
                            abortSession
                        else
                            consume_continue


                    frame@(NH2.Frame _ (NH2.SettingsFrame settings_list) ) -> do
                        -- Increase all the stuff....
                        case find (\(i,_) -> i == NH2.SettingsInitialWindowSize) settings_list of

                            Just (_, new_default_stream_size) -> do
                                old_default_stream_size_mvar <- view defaultStreamWindow
                                old_default_stream_size <- liftIO $ takeMVar old_default_stream_size_mvar
                                let general_delta = new_default_stream_size - old_default_stream_size
                                stream_to_flow <- view stream2flow
                                -- Add capacity to everybody's windows
                                liftIO . withMVar stream_to_flow $ \ stream_to_flow' ->
                                    H.mapM_ (\ (k,v) ->
                                                 when (k /=0 ) $ putMVar v (AddBytes_FCM $! general_delta)
                                            )
                                            stream_to_flow'


                                -- And set a new value
                                liftIO $ putMVar old_default_stream_size_mvar $! new_default_stream_size


                            Nothing ->
                                -- This is a silenced internal error
                                return ()

                        -- And send the frame down to the session, so that session specific settings
                        -- can be applied.
                        liftIO $ sendToSession starting $! frame
                        consume_continue

                    a_frame@(NH2.Frame (NH2.FrameHeader _ _ stream_id) _ )   -> do
                        -- Update the keep of last stream
                        -- lift . startStreamOutputQueueIfNotExists (NH2.fromStreamIdentifier stream_id) priority
                        lift . updateLastInputStream $ stream_id

                        -- Send frame to the session
                        liftIO $ sendToSession starting a_frame

                        -- tail recursion: go again...
                        consume_continue

            Nothing    ->
                -- We may as well exit this thread
               return ()


-- All the output frames come this way first
outputGatherer :: SessionOutput -> FramerSession ()
outputGatherer session_output = do
    frame_sent_report_callback <- view $
       sessionsContext            .
       sessionsConfig             .
       sessionsCallbacks          .
       dataDeliveryCallback_SC

    session_id <- view sessionIdAtFramer

    let

       dataForFrame p1 p2 =
           LB.fromStrict $ NH2.encodeFrame p1 p2

       cont = loopPart session_id frame_sent_report_callback

       loopPart :: Int -> Maybe DataFrameDeliveryCallback -> FramerSession ()
       loopPart session_id frame_sent_report_callback = do
           command_or_frame  <- liftIO $ getFrameFromSession session_output
           case command_or_frame of

               Left (CancelSession_SOC error_code) -> do
                   -- The session wants to cancel things as harshly as possible, send a GoAway frame with
                   -- the information I have here.
                   sendGoAwayFrame error_code
                   releaseFramer
                   -- And this causes this thread to finish, so that no new frames
                   -- are taken from the session. Correspondingly, an exception is raised in
                   -- the session if it tries to write another frame

               Left (SpecificTerminate_SOC last_stream_id) -> do
                   -- This is used when the session wants to finish in a specific way.
                   sendSpecificTerminateGoAway last_stream_id
                   releaseFramer

               Left (FinishStream_SOC stream_id ) -> do
                   -- Session knows that we are done with the given stream, and that we can release
                   -- the flow control structures
                   finishFlowControlForStream stream_id
                   cont

               Right ( p1@(NH2.EncodeInfo _ stream_idii _), p2@(NH2.DataFrame _), ef ) -> do
                   -- This frame is flow-controlled... I may be unable to send this frame in
                   -- some circumstances...
                   let
                       stream_id = stream_idii
                       priority = fromMaybe stream_id $ ef ^. priorityEffect_Ef

                   startStreamOutputQueueIfNotExists stream_id $ priority

                   stream2output_mvar <- view stream2outputBytes
                   lookup_result <- liftIO $ withMVar stream2output_mvar $ \ s2o -> H.lookup s2o stream_id
                   (stream_bytes_chan, frame_ordinal_mvar) <- case lookup_result of
                       Nothing ->
                           error "It is the end of the world at Framer.hs"
                       Just x -> return x

                   -- All the dance below is to avoid a system call if there is no need
                   case (frame_sent_report_callback, ef ^. fragmentDeliveryCallback_Ef ) of
                       (Just c1, Just c2) -> liftIO $ do
                           -- Here we invoke the client's callback.
                           when_delivered <- getTime Monotonic
                           ordinal <- modifyMVar frame_ordinal_mvar $ \ o -> return (o+1, o)
                           c1 session_id stream_id ordinal when_delivered
                           c2 ordinal when_delivered
                       (Nothing, Just c2) -> liftIO $ do
                           when_delivered <- getTime Monotonic
                           ordinal <- modifyMVar frame_ordinal_mvar $ \ o -> return (o+1, o)
                           c2 ordinal when_delivered
                       (Just c1, Nothing) -> liftIO $ do
                           when_delivered <- getTime Monotonic
                           ordinal <- modifyMVar frame_ordinal_mvar $ \ o -> return (o+1, o)
                           c1 session_id stream_id ordinal when_delivered
                       (Nothing, Nothing) -> return ()

                   liftIO $ putMVar stream_bytes_chan $! dataForFrame p1 p2
                   cont


               Right (p1, p2@(NH2.PushPromiseFrame _ _), _effect ) -> do
                   handleHeadersOfStream p1 p2
                   cont

               Right (p1, p2@(NH2.HeadersFrame _ _), _effect ) -> do
                   handleHeadersOfStream p1 p2
                   cont

               Right (p1, p2@(NH2.ContinuationFrame _), _effect ) -> do
                   handleHeadersOfStream p1 p2
                   cont

               Right (p1, p2, _effect) -> do
                   -- Most other frames go right away... as long as no headers are in process...
                   no_headers <- view noHeadersInChannel
                   liftIO $ takeMVar no_headers
                   pushFrame p1 p2
                   liftIO $ putMVar no_headers NoHeadersInChannel
                   cont

    -- We start by sending a settings frame
    pushFrame
        (NH2.EncodeInfo NH2.defaultFlags 0 Nothing)
        (NH2.SettingsFrame [])

    -- And then we continue...
    loopPart session_id frame_sent_report_callback


updateLastInputStream :: GlobalStreamId  -> FramerSession ()
updateLastInputStream stream_id = do
    last_stream_id_mvar <- view lastInputStream
    liftIO $ modifyMVar_ last_stream_id_mvar (\ x -> return $ max x stream_id)


updateLastOutputStream :: GlobalStreamId  -> FramerSession ()
updateLastOutputStream stream_id = do
    last_stream_id_mvar <- view lastOutputStream
    liftIO $ modifyMVar_ last_stream_id_mvar (\ x -> return $ max x stream_id)


startStreamOutputQueueIfNotExists :: GlobalStreamId -> Int -> FramerSession ()
startStreamOutputQueueIfNotExists stream_id priority = do
    table_mvar <- view stream2flow
    val <- liftIO . withMVar table_mvar  $ \ table -> H.lookup table stream_id
    case val of
        Nothing | stream_id /= 0 -> do
            startStreamOutputQueue stream_id priority
            return ()

        _ ->
            return ()


-- Handles only Data frames.
startStreamOutputQueue :: Int -> Int -> FramerSession (MVar LB.ByteString, MVar FlowControlCommand)
startStreamOutputQueue stream_id priority = do
    -- New thread for handling outputs of this stream is needed
    bytes_chan   <- liftIO newEmptyMVar
    ordinal_num  <- liftIO $ newMVar 0
    command_chan <- liftIO newEmptyMVar

    s2o_mvar <- view stream2outputBytes

    liftIO . withMVar s2o_mvar $ {-# SCC e1  #-}  \ s2o ->  H.insert s2o stream_id (bytes_chan, ordinal_num)

    stream2flow_mvar <- view stream2flow


    liftIO . withMVar stream2flow_mvar  $  {-# SCC e2  #-}  \ s2c ->  H.insert s2c stream_id command_chan
    --
    initial_cap_mvar <- view defaultStreamWindow
    initial_cap <- liftIO $ readMVar initial_cap_mvar
    close_action <- view closeAction
    sessions_context <- view sessionsContext
    session_id' <- view sessionIdAtFramer
    output_is_forbidden_mvar <- view outputIsForbidden

    -- And don't forget the thread itself
    let
        close_on_error session_id session_context comp =
                (E.catch
                    comp
                    (exc_handler session_id session_context)
                )

        exc_handler :: Int -> SessionsContext -> IOProblem -> IO ()
        exc_handler x y e = do
            -- Let's also decree that other streams don't even try
            modifyMVar_ output_is_forbidden_mvar ( \ _ -> return True)
            sessionExceptionHandler Framer_HTTP2SessionComponent x y e
            close_action


    read_state <- ask
    liftIO $ forkIO $ close_on_error session_id' sessions_context  $ runReaderT
        ({-# SCC forkFlowControlOutput  #-} flowControlOutput stream_id priority initial_cap 0 "" command_chan bytes_chan)
        read_state

    return (bytes_chan , command_chan)


-- This works in the output side of the HTTP/2 framing session, and it acts as a
-- semaphore ensuring that headers are output without any interleaved frames.
--
-- There are more synchronization mechanisms in Http2.Session that ensure we
-- only get here frames from one and the same stream.
handleHeadersOfStream :: NH2.EncodeInfo -> NH2.FramePayload -> FramerSession ()
handleHeadersOfStream p1@(NH2.EncodeInfo {}) frame_payload
    | frameIsHeadersAndOpensStream frame_payload && not (frameEndsHeaders p1 frame_payload) = do
        -- Take it
        no_headers <- view noHeadersInChannel
        liftIO $ takeMVar no_headers
        pushFrame p1 frame_payload
        -- Don't put the MvAR HERE

    | frameIsHeadersAndOpensStream frame_payload && frameEndsHeaders p1 frame_payload = do
        no_headers <- view noHeadersInChannel
        liftIO $ takeMVar no_headers
        pushFrame p1 frame_payload
        -- Since we finish....
        liftIO $ putMVar no_headers NoHeadersInChannel

    | frameEndsHeaders p1 frame_payload = do
        -- I can only get here for a continuation frame  after something else that is a headers
        no_headers <- view noHeadersInChannel
        pushFrame p1 frame_payload
        liftIO $ putMVar no_headers NoHeadersInChannel
        return ()

    | otherwise =
        -- Nothing to do with the mvar, the no_headers should be empty
        pushFrame p1 frame_payload


-- Used to know when we need to switch the channel to "exclusive" mode.
frameIsHeadersAndOpensStream :: NH2.FramePayload -> Bool
frameIsHeadersAndOpensStream (NH2.HeadersFrame _  _ )      = True
frameIsHeadersAndOpensStream (NH2.PushPromiseFrame _ _)    = True
frameIsHeadersAndOpensStream _                             = False


frameEndsHeaders  :: NH2.EncodeInfo -> NH2.FramePayload -> Bool
frameEndsHeaders (NH2.EncodeInfo flags _ _) (NH2.HeadersFrame _ _) = NH2.testEndHeader flags
frameEndsHeaders (NH2.EncodeInfo flags _ _) (NH2.ContinuationFrame _) = NH2.testEndHeader flags
frameEndsHeaders (NH2.EncodeInfo flags _ _) (NH2.PushPromiseFrame _ _) = NH2.testEndHeader flags
frameEndsHeaders _ _ = False


-- Push a frame into the output channel... this waits for the
-- channel to be free to send.
pushFrame :: NH2.EncodeInfo
             -> NH2.FramePayload -> FramerSession ()
pushFrame p1 p2 = do
    let bs = LB.fromStrict $ NH2.encodeFrame p1 p2
    sendBytes bs


pushPrefix :: FramerSession ()
pushPrefix = do
    let bs = LB.fromStrict NH2.connectionPreface
    sendBytes bs


-- Default sendGoAwayFrame. This one assumes that actions are taken as soon as stream is
-- received. This is a good default strategy for streams that cause things to happen, that
-- is, the ones with POST and GET.
sendGoAwayFrame :: NH2.ErrorCodeId -> FramerSession ()
sendGoAwayFrame error_code = do
    last_stream_id_mvar <- view lastInputStream
    last_stream_id <- liftIO $ readMVar last_stream_id_mvar
    pushFrame
        (NH2.EncodeInfo NH2.defaultFlags 0 Nothing)
        (NH2.GoAwayFrame last_stream_id error_code "")


sendSpecificTerminateGoAway :: GlobalStreamId -> FramerSession ()
sendSpecificTerminateGoAway last_stream =
    pushFrame
        (NH2.EncodeInfo NH2.defaultFlags 0 Nothing)
        (NH2.GoAwayFrame last_stream NH2.NoError "")


-- From this point on data is really serialized,
sendBytes :: LB.ByteString -> FramerSession ()
sendBytes bs = do
    push_action <- view pushAction
    -- I don't think I need to lock here...
    can_output <- view canOutput
    liftIO $
        bs `seq`
            C.bracket
                (takeMVar   can_output)
                (const $ push_action bs)
                (putMVar can_output)


-- A thread in charge of doing flow control transmission....This sends already
-- formatted frames (ByteStrings), not the frames themselves. And it doesn't
-- mess with the structure of the packets.
--
-- There is one of these for each stream
--
flowControlOutput :: Int
                     -> Int
                     -> Int
                     -> Int
                     ->  LB.ByteString
                     -> MVar FlowControlCommand
                     -> MVar LB.ByteString
                     ->  FramerSession ()
flowControlOutput stream_id priority capacity ordinal leftovers commands_chan bytes_chan =
    ordinal `seq` if leftovers == ""
      then {-# SCC fcOBranch1  #-} do
        -- Get more data (possibly block waiting for it)
        bytes_to_send <- liftIO $ {-# SCC perfectlyHarmlessExceptionPoint #-} takeMVar bytes_chan
        flowControlOutput stream_id priority capacity ordinal  bytes_to_send commands_chan bytes_chan
      else {-# SCC fcOBranch2  #-}  do
        -- Length?
        let amount = fromIntegral  (LB.length leftovers - 9)
        if  amount <= capacity
          then do
            -- Is
            -- I can send ... if no headers are in process....
            -- liftIO . logit $ "set-priority (stream_id, prio, ordinal) " `mappend` (pack . show) (__stream_id, priority, ordinal)
            withPrioritySend priority stream_id ordinal leftovers
            flowControlOutput  stream_id priority (capacity - amount) (ordinal+1 ) "" commands_chan bytes_chan
          else do
            -- I can not send because flow-control is full, wait for a command instead
            command <- liftIO $ {-# SCC t2 #-} takeMVar commands_chan
            case  {-# SCC t3 #-} command of
                AddBytes_FCM delta_cap ->
                    -- liftIO $ putStrLn $ "Flow control delta_cap stream " ++ (show stream_id)
                    flowControlOutput stream_id priority (capacity + delta_cap) ordinal leftovers commands_chan bytes_chan


releaseFramer :: FramerSession ()
releaseFramer =
    -- Release any resources pending...
    -- This is not needed as of right now, since garbage collection works well.
    -- I'm leaving it here just in case we need to re-activate it in the future.
    return ()


-- This prioritizes DATA packets, in some rudimentary way.
-- This code will be replaced in due time for something compliant.
withPrioritySend :: Int -> Int -> Int -> LB.ByteString -> FramerSession ()
withPrioritySend priority stream_id packet_ordinal datum = do
    PrioritySendState {_semToSend = s, _prioQ = pqm } <- view prioritySendState
    let
        new_record = PrioPacket  ( (priority, stream_id, packet_ordinal), datum)
    -- Now, for this to work, I need to have enough capacity to add something to the queue
    liftIO $ do
        -- We are using a semaphore to avoid overflowing this place. Notice that the flow
        -- control output is till using an unbounded queue!!!
        MS.wait s
        -- And add it to the queue....
        STM.atomically $ do
            pq <- takeTMVar pqm
            putTMVar pqm $ PQ.insert new_record pq


-- In charge of actually sending the data frames, in a special thread (create in the caller)
sendReordering :: FramerSession ()
sendReordering = do
    PrioritySendState {_semToSend = s, _prioQ = pqm } <- view prioritySendState
    no_headers <- view noHeadersInChannel
    -- Get a packet, if possible, or wait for it
    PrioPacket ( (priority_, stream_id, packed_ordinal_), datum) <- liftIO . STM.atomically $ do
        pq <- takeTMVar pqm
        if PQ.null pq
          then STM.retry
          else do
            let
              (record, thinner) =  PQ.deleteFindMin pq
            putTMVar pqm thinner
            return record
    -- liftIO . logit $ "prio-send (prio,stream_id, ordinal) " `mappend` (pack . show) (priority, stream_id, packed_ordinal_)
    -- Since I got something to send, I can let one of the guys to put more stuff
    liftIO $ MS.signal s
    -- Now we do the tries and locks for headers
    C.bracket
        (liftIO $ takeMVar no_headers)
        (\ _ -> liftIO $ putMVar no_headers NoHeadersInChannel)
        (\ _ -> sendBytes datum )

    -- Sleep for a little bit, we dont' want too many frames
    -- here too fast. BIG TODO: We need a better way to handle this
    liftIO $ replicateM_ 10 CC.yield
    --liftIO $ threadDelay 100

    -- And tail-recurse
    sendReordering
