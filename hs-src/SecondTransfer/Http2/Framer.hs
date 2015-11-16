
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

import           Control.Exception
import qualified Control.Exception                      as E
import           Control.Lens                           (view, (^.) )
import qualified Control.Lens                           as L
import           Control.Monad                          (unless, when)
import           Control.Monad.IO.Class                 (liftIO)
--import qualified Control.Monad.Catch                    as C
import           Control.Monad.Trans.Class              (lift)
-- import           Control.DeepSeq                        (($!!))
import           Control.Monad.Trans.Reader
import           Data.Binary                            (decode)
import qualified Data.ByteString                        as B
--import           Data.ByteString.Char8                  (pack)
import qualified Data.ByteString.Lazy                   as LB
import qualified Data.ByteString.Builder                as Bu
import           Data.Conduit
import           Data.Foldable                          (find)
--import qualified Data.PQueue.Min                        as PQ
import           Data.Maybe                             (fromMaybe)

import qualified Network.HTTP2                          as NH2

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
import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.Utils                   (Word24, word24ToInt)
import           SecondTransfer.Exception

import           SecondTransfer.Http2.TransferTypes
import           SecondTransfer.Http2.OutputTray

#ifdef SECONDTRANSFER_MONITORING
import           SecondTransfer.MainLoop.Logging        (logit)
#endif

--import           Debug.Trace                            (trace)


http2PrefixLength :: Int
http2PrefixLength = B.length NH2.connectionPreface

-- Let's do flow control here here ....

type HashTable k v = H.CuckooHashTable k v


data FlowControlCommand =
     AddBytes_FCM Int
--    |Finish_FCM

-- A hashtable from stream id to channel of availabiliy increases
type Stream2AvailSpace = HashTable GlobalStreamId (MVar FlowControlCommand)



-- Simple thread to prioritize frames in the session
data PrioritySendState = PrioritySendState {
     _outputTray_PSS                 :: MVar OutputTray
   , _dataReady_PSS                  :: MVar ()
   , _spaceReady_PSS                 :: MVar ()
     }

L.makeLenses ''PrioritySendState


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

    -- Flag that says if the session has been unwound... if such,
    -- threads are adviced to exit as early as possible
    , _outputIsForbidden     :: MVar Bool

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
    --, _lastOutputStream      :: MVar Int

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
    last_stream_id            <- newMVar 0
    -- last_output_stream_id     <- newMVar 0
    output_is_forbidden       <- newMVar False

    output_tray_mvar          <- newMVar . newOutputTray . ( ^. sessionsConfig . trayMaxSize ) $ sessions_context
    data_ready_mvar           <- newEmptyMVar
    space_ready_mvar          <- newMVar ()

    -- We need some shared state
    let framer_session_data = FramerSessionData {
        _stream2flow          = stream2flow_mvar
        ,_stream2outputBytes  = stream2output_bytes_mvar
        ,_defaultStreamWindow = default_stream_size_mvar
        ,_pushAction          = push_action
        ,_closeAction         = close_action
        ,_sessionIdAtFramer   = new_session_id
        ,_sessionsContext     = sessions_context
        ,_lastInputStream     = last_stream_id
        --,_lastOutputStream    = last_output_stream_id
        ,_outputIsForbidden   = output_is_forbidden
        ,_prioritySendState   = PrioritySendState {
                                  _outputTray_PSS = output_tray_mvar
                                , _dataReady_PSS = data_ready_mvar
                                , _spaceReady_PSS = space_ready_mvar
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

        -- TODO:  I don't think much is being done here
        exc_handler :: Int -> SessionsContext -> FramerException -> IO ()
        exc_handler x y e = do
            modifyMVar_ output_is_forbidden (\ _ -> return True)
            sessionExceptionHandler Framer_HTTP2SessionComponent x y e

        io_exc_handler :: Int -> SessionsContext -> IOProblem -> IO ()
        io_exc_handler _x _y _e =
            modifyMVar_ output_is_forbidden (\ _ -> return True)
            -- !!! These exceptions are way too common for we to care....
            -- errorM "HTTP2.Framer" "Exception went up"
            -- sessionExceptionHandler Framer_HTTP2SessionComponent x y e

    _ <- forkIOExc "inputGathererHttp2"
        $ close_on_error new_session_id sessions_context
        $ ignoreException blockedIndefinitelyOnMVar ()
        $ runReaderT (inputGatherer pull_action session_input ) framer_session_data
    _ <- forkIOExc "outputGathererHttp2"
        $ close_on_error new_session_id sessions_context
        $ ignoreException blockedIndefinitelyOnMVar ()
        $ runReaderT (outputGatherer session_output ) framer_session_data
    -- Actual data is reordered before being sent
    _ <- forkIOExc "sendReorderingHttp2"
        $ close_on_error new_session_id sessions_context
        $ ignoreException blockedIndefinitelyOnMVar ()
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
addCapacity 0         _delta_cap =
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

                Just _command_chan -> do
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

        output_is_forbidden_mvar <- view outputIsForbidden
        output_is_forbidden <- liftIO $ readMVar output_is_forbidden_mvar

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

    session_role <- view sessionRole_FSD

    -- When acting as a client, the first step is to send the prefix...
    when (session_role == Client_SR) $
        pushPrefix

    frame_sent_report_callback <- view $
       sessionsContext            .
       sessionsConfig             .
       sessionsCallbacks          .
       dataDeliveryCallback_SC

    session_id <- view sessionIdAtFramer
    let

       dataForFrame p1 p2 =
           LB.fromStrict $ NH2.encodeFrame p1 p2

       cont = loopPart

       loopPart ::  FramerSession ()
       loopPart  = do
           command_or_frame  <- liftIO $ getFrameFromSession session_output
           case command_or_frame of

               Command_StFB (CancelSession_SOC error_code) -> do
                   -- The session wants to cancel things as harshly as possible, send a GoAway frame with
                   -- the information I have here.
                   sendGoAwayFrame error_code
                   releaseFramer
                   -- And this causes this thread to finish, so that no new frames
                   -- are taken from the session. Correspondingly, an exception is raised in
                   -- the session if it tries to write another frame

               Command_StFB (SpecificTerminate_SOC last_stream_id error_code) -> do
                   -- This is used when the session wants to finish in a specific way.
                   sendSpecificTerminateGoAway last_stream_id error_code
                   releaseFramer

               Command_StFB (FinishStream_SOC stream_id ) -> do
                   -- Session knows that we are done with the given stream, and that we can release
                   -- the flow control structures
                   finishFlowControlForStream stream_id
                   cont

               DataFrame_StFB ( p1@(NH2.EncodeInfo _ stream_idii _), p2@(NH2.DataFrame _), ef ) -> do
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


               PriorityTrain_StFB frames -> do
                   -- Serialize the entire train
                   let
                       bs = serializeMany frames
                   -- Put it in the output tray
                   withHighPrioritySend bs
                   -- and continue
                   cont

               case_ -> error $ "Error: case not spefified " ++ (show case_)

    -- We start by sending a settings frame
    pushControlFrame
        (NH2.EncodeInfo NH2.defaultFlags 0 Nothing)
        (NH2.SettingsFrame [])

    -- And then we continue...
    loopPart


updateLastInputStream :: GlobalStreamId  -> FramerSession ()
updateLastInputStream stream_id = do
    last_stream_id_mvar <- view lastInputStream
    liftIO $ modifyMVar_ last_stream_id_mvar (\ x -> return $ max x stream_id)


-- updateLastOutputStream :: GlobalStreamId  -> FramerSession ()
-- updateLastOutputStream stream_id = do
--     last_stream_id_mvar <- view lastOutputStream
--     liftIO $ modifyMVar_ last_stream_id_mvar (\ x -> return $ max x stream_id)


startStreamOutputQueueIfNotExists :: GlobalStreamId -> Int -> FramerSession ()
startStreamOutputQueueIfNotExists stream_id priority = do
    table_mvar <- view stream2flow
    val <- liftIO . withMVar table_mvar  $ \ table -> H.lookup table stream_id
    case val of
        Nothing | stream_id /= 0 -> do
            _ <- startStreamOutputQueue stream_id priority
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
    _ <- liftIO $ forkIOExc "streamOutputQueue"
           $ ignoreException blockedIndefinitelyOnMVar  ()
           $ close_on_error session_id' sessions_context
           $ runReaderT (flowControlOutput stream_id priority initial_cap 0 "" command_chan bytes_chan)
             read_state

    return (bytes_chan , command_chan)


pushPrefix :: FramerSession ()
pushPrefix = do
    let bs = LB.fromStrict NH2.connectionPreface
    -- We will send the data, mixing as needed, with the incredible priority
    -- of -2
    withPrioritySend_ (-20) 0 0 0 bs


-- Default sendGoAwayFrame. This one assumes that actions are taken as soon as stream is
-- received. This is a good default strategy for streams that cause things to happen, that
-- is, the ones with POST and GET.
sendGoAwayFrame :: NH2.ErrorCodeId -> FramerSession ()
sendGoAwayFrame error_code = do
    last_stream_id_mvar <- view lastInputStream
    last_stream_id <- liftIO $ readMVar last_stream_id_mvar
    pushControlFrame
        (NH2.EncodeInfo NH2.defaultFlags 0 Nothing)
        (NH2.GoAwayFrame last_stream_id error_code "")


sendSpecificTerminateGoAway :: GlobalStreamId -> NH2.ErrorCodeId -> FramerSession ()
sendSpecificTerminateGoAway last_stream error_code =
    pushControlFrame
        (NH2.EncodeInfo NH2.defaultFlags 0 Nothing)
        (NH2.GoAwayFrame last_stream error_code "")


-- Only one caller to this: the output tray functionality!!
sendBytesN :: LB.ByteString -> FramerSession ()
sendBytesN bs = do
    push_action <- view pushAction
    -- I don't think I need to lock here...
    liftIO $ push_action bs


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
        -- Get more data (possibly block waiting for it)... there will be an
        -- exception here from time to time...
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
            withNormalPrioritySend priority stream_id ordinal leftovers
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
--
--   System priority:
--        -20 : the prefix, absolutely most important guy
--        -1 : All packets except data  packets
--         0 : data packets.
withPrioritySend_ :: Int -> Int -> Int -> Int -> LB.ByteString -> FramerSession ()
withPrioritySend_ system_priority priority stream_id packet_ordinal datum = do
    pss <- view prioritySendState
    let
        new_entry = TrayEntry {
            _systemPriority_TyE = system_priority -- Ordinary data packets go after everybody else
          , _streamPriority_TyE = priority
          , _streamOrdinal_TyE = packet_ordinal
          , _payload_TyE =  datum
          , _streamId_TyE = stream_id
            }
        attempt =  do
            could_add <- modifyMVar (pss ^. outputTray_PSS) $ \ ot1 -> do
                _ <- tryTakeMVar (pss ^. spaceReady_PSS)
                if (ot1 ^. filling_OuT) < (ot1 ^. maxLength_OuT)
                  then do
                    let new_ot = addEntry ot1 new_entry
                    return (new_ot, True)
                  else do
                    return (ot1, False)
            if could_add
              then do
                _ <- tryPutMVar (pss ^. dataReady_PSS) ()
                return ()
              else do
                readMVar (pss ^. spaceReady_PSS)
                attempt
    -- Now, for this to work, I need to have enough capacity to add something to the queue
    liftIO $ attempt


withNormalPrioritySend ::  Int -> Int -> Int -> LB.ByteString -> FramerSession ()
withNormalPrioritySend = withPrioritySend_ 0


withHighPrioritySend :: LB.ByteString -> FramerSession ()
withHighPrioritySend  datum = withPrioritySend_ (-1) 0 0 0 datum


serializeMany :: [OutputFrame] -> LB.ByteString
serializeMany frames =
    Bu.toLazyByteString $ mconcat (map (\ (a,b,_) -> Bu.byteString $ NH2.encodeFrame a b ) frames)

-- Needed here and there
pushControlFrame :: NH2.EncodeInfo -> NH2.FramePayload -> FramerSession ()
pushControlFrame frame_encode_info frame_payload =
  do
    let datum = LB.fromStrict $ NH2.encodeFrame frame_encode_info frame_payload
    withHighPrioritySend datum


-- In charge of actually sending the  frames, in a special thread (create said thread
-- in the caller)
sendReordering :: FramerSession ()
sendReordering = do
    pss <- view prioritySendState
    use_size <- view (sessionsContext . sessionsConfig . networkChunkSize)
    -- Get a set of packets (that is, their reprs) to send
    let
        get_sendable_data = do
            maybe_entries <- modifyMVar (pss ^. outputTray_PSS) $ \ ot1 -> do
                _ <- tryTakeMVar (pss ^. dataReady_PSS)
                if (ot1 ^. filling_OuT) <= 0
                  then
                    return (ot1 ,Nothing)
                  else do
                    let
                      (ot2, entries) = splitOverSize use_size ot1
                    return (ot2, Just entries)
            case maybe_entries of
                Just entries -> do
                    _ <- tryPutMVar (pss ^. spaceReady_PSS) ()
                    return . Bu.toLazyByteString . mconcat . map (Bu.lazyByteString . ( ^. payload_TyE) ) $  entries
                Nothing -> do
                    _ <- readMVar (pss ^. dataReady_PSS)
                    get_sendable_data
    entries_data <- liftIO get_sendable_data
    sendBytesN entries_data
    sendReordering
