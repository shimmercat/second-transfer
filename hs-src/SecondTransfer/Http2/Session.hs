-- Session: links frames to streams, and helps in ordering the header frames
-- so that they don't get mixed with header frames from other streams when
-- resources are being served concurrently.
{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http2.Session(
    http2Session
    ,getFrameFromSession
    ,sendFirstFrameToSession
    ,sendMiddleFrameToSession
    ,sendCommandToSession

    ,CoherentSession
    ,SessionInput(..)
    ,SessionInputCommand(..)
    ,SessionOutput(..)
    ,SessionOutputCommand(..)
    ,SessionCoordinates(..)
    ,SessionComponent(..)
    ,SessionsCallbacks(..)
    ,SessionsConfig(..)
    ,ErrorCallback

    -- Internal stuff
    ,OutputFrame
    ,InputFrame
    ) where

#include "Logging.cpphs"

-- System grade utilities
import           Control.Concurrent                     (ThreadId, forkIO)
import           Control.Concurrent.Chan
import           Control.Exception                      (throwTo)
import qualified Control.Exception                      as E
import           Control.Monad                          (forever)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Reader
-- import           Control.Monad.Catch                    (throwM)

import           Control.Concurrent.MVar
import qualified Data.ByteString                        as B
import           Data.ByteString.Char8                  (pack,unpack)
import qualified Data.ByteString.Builder                as Bu
import qualified Data.ByteString.Lazy                   as Bl
import           Data.Conduit
import qualified Data.HashTable.IO                      as H
import qualified Data.IntSet                            as NS
import           Data.Maybe                             (isJust)
#ifndef IMPLICIT_MONOID
import           Data.Monoid                            (mappend)
#endif

import           Control.Lens

-- No framing layer here... let's use Kazu's Yamamoto library
import qualified Network.HPACK                          as HP
import qualified Network.HTTP2                          as NH2

-- Logging utilities
import           System.Log.Logger

-- Imports from other parts of the program
import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.MainLoop.Tokens
import           SecondTransfer.Sessions.Config
import           SecondTransfer.Sessions.Internal       (sessionExceptionHandler, SessionsContext, sessionsConfig)
import           SecondTransfer.Utils                   (unfoldChannelAndSource)
import           SecondTransfer.Exception
import qualified SecondTransfer.Utils.HTTPHeaders       as He
import           SecondTransfer.MainLoop.Logging        (logWithExclusivity, logit)

-- Unfortunately the frame encoding API of Network.HTTP2 is a bit difficult to
-- use :-(
type OutputFrame = (NH2.EncodeInfo, NH2.FramePayload)
type InputFrame  = NH2.Frame


useChunkLength :: Int
useChunkLength = 2048


-- Singleton instance used for concurrency
data HeadersSent = HeadersSent

-- All streams put their data bits here. A "Nothing" value signals
-- end of data.
type DataOutputToConveyor = (GlobalStreamId, Maybe B.ByteString)


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
    ,_dataOutput :: MVar DataOutputToConveyor

    ,_streamsCancelled_WTE :: MVar NS.IntSet

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

type SessionOutputPacket = Either SessionOutputCommand OutputFrame

type SessionOutputChannelAbstraction = Chan SessionOutputPacket

-- From outside, one can only read from this one
newtype SessionOutput = SessionOutput SessionOutputChannelAbstraction
getFrameFromSession :: SessionOutput -> IO SessionOutputPacket
getFrameFromSession (SessionOutput chan) = readChan chan


type HashTable k v = H.CuckooHashTable k v


type Stream2HeaderBlockFragment = HashTable GlobalStreamId Bu.Builder


type WorkerMonad = ReaderT WorkerThreadEnvironment IO


-- Have to figure out which are these...but I would expect to have things
-- like unexpected aborts here in this type.
data SessionInputCommand =
    FirstFrame_SIC InputFrame       -- This frame is special
    |MiddleFrame_SIC InputFrame     -- Ordinary frame
    |InternalAbort_SIC              -- Internal abort from the session itself
    |CancelSession_SIC              -- Cancel request from the framer
  deriving Show


-- temporary
data  SessionOutputCommand =
    CancelSession_SOC
  deriving Show


-- Here is how we make a session
type SessionMaker = SessionsContext -> IO Session


-- Here is how we make a session wrapping a CoherentWorker
type CoherentSession = CoherentWorker -> SessionMaker


data PostInputMechanism = PostInputMechanism (MVar (Maybe B.ByteString), InputDataStream)

-- Settings imposed by the peer
data SessionSettings = SessionSettings {
    _pushEnabled :: Bool
    }

makeLenses ''SessionSettings


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
    ,_lastGoodStream             :: MVar Int

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

    -- Worker thread register. This is a dictionary from stream id to
    -- the ThreadId of the thread with the worker thread. I use this to
    -- raise asynchronous exceptions in the worker thread if the stream
    -- is cancelled by the client. This way we get early finalization.
    ,_stream2WorkerThread        :: HashTable Int ThreadId

    -- Use to retrieve/set the session id
    ,_sessionIdAtSession         :: Int

    -- And used to keep peer session settings
    ,_sessionSettings            :: MVar SessionSettings
    }


makeLenses ''SessionData


--                                v- {headers table size comes here!!}
http2Session :: CoherentWorker -> Int -> SessionsContext -> IO Session
http2Session coherent_worker session_id sessions_context =   do
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
    headers_output            <- newChan :: IO (Chan (GlobalStreamId, MVar HeadersSent, Headers))
    data_output               <- newEmptyMVar :: IO (MVar DataOutputToConveyor)

    stream2postinputmechanism <- H.new
    stream2workerthread       <- H.new
    last_good_stream_mvar     <- newMVar (-1)

    receiving_headers         <- newMVar Nothing
    session_settings          <- newMVar $ SessionSettings { _pushEnabled = True }

    -- What about stream cancellation?
    cancelled_streams_mvar    <- newMVar $ NS.empty :: IO (MVar NS.IntSet)

    let for_worker_thread = WorkerThreadEnvironment {
        _streamId = error "NotInitialized"
        ,_headersOutput = headers_output
        ,_dataOutput = data_output
        ,_streamsCancelled_WTE = cancelled_streams_mvar
        }

    let session_data  = SessionData {
        _sessionsContext             = sessions_context
        ,_sessionInput               = session_input
        ,_sessionOutput              = session_output_mvar
        ,_toDecodeHeaders            = decode_headers_table_mvar
        ,_toEncodeHeaders            = encode_headers_table_mvar
        ,_stream2HeaderBlockFragment = stream_request_headers
        ,_forWorkerThread            = for_worker_thread
        ,_coherentWorker             = coherent_worker
        ,_streamsCancelled           = cancelled_streams_mvar
        ,_stream2PostInputMechanism  = stream2postinputmechanism
        ,_stream2WorkerThread        = stream2workerthread
        ,_sessionIdAtSession         = session_id
        ,_receivingHeaders           = receiving_headers
        ,_sessionSettings            = session_settings
        ,_lastGoodStream             = last_good_stream_mvar
        }

    let
        exc_handler :: SessionComponent -> HTTP2SessionException -> IO ()
        exc_handler component e = sessionExceptionHandler component session_id sessions_context e
        exc_guard :: SessionComponent -> IO () -> IO ()
        exc_guard component action = E.catch
            action
            (\e -> do
                INSTRUMENTATION( errorM "HTTP2.Session" "Exception processed" )
                exc_handler component e
            )

    -- Create an input thread that decodes frames...
    forkIO $ exc_guard SessionInputThread_HTTP2SessionComponent
           $ runReaderT sessionInputThread session_data

    -- Create a thread that captures headers and sends them down the tube
    forkIO $ exc_guard SessionHeadersOutputThread_HTTP2SessionComponent
           $ runReaderT (headersOutputThread headers_output session_output_mvar) session_data

    -- Create a thread that captures data and sends it down the tube
    forkIO $ exc_guard SessionDataOutputThread_HTTP2SessionComponent
           $ dataOutputThread data_output session_output_mvar

    -- The two previous threads fill the session_output argument below (they write to it)
    -- the session machinery in the other end is in charge of sending that data through the
    -- socket.

    return ( (SessionInput session_input),
             (SessionOutput session_output) )


-- TODO: Some ill clients can break this thread with exceptions. Make these paths a bit
--- more robust.
sessionInputThread :: ReaderT SessionData IO ()
sessionInputThread  = do
    INSTRUMENTATION( debugM "HTTP2.Session" "Entering sessionInputThread" )

    -- This is an introductory and declarative block... all of this is tail-executed
    -- every time that  a packet needs to be processed. It may be a good idea to abstract
    -- these values in a closure...
    session_input             <- view sessionInput

    decode_headers_table_mvar <- view toDecodeHeaders
    stream_request_headers    <- view stream2HeaderBlockFragment
    cancelled_streams_mvar    <- view streamsCancelled
    coherent_worker           <- view coherentWorker

    for_worker_thread_uns     <- view forWorkerThread
    stream2workerthread       <- view stream2WorkerThread
    receiving_headers_mvar    <- view receivingHeaders
    last_good_stream_mvar     <- view lastGoodStream

    input                     <- liftIO $ readChan session_input

    case input of

        FirstFrame_SIC (NH2.Frame
            (NH2.FrameHeader _ 1 null_stream_id ) _ )| NH2.toStreamIdentifier 0 == null_stream_id  -> do
            -- This is a SETTINGS ACK frame, which is okej to have,
            -- do nothing here
            continue

        FirstFrame_SIC
            (NH2.Frame
                (NH2.FrameHeader _ 0 null_stream_id )
                (NH2.SettingsFrame settings_list)
            ) | NH2.toStreamIdentifier 0 == null_stream_id  -> do
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

        -- The block below will process both HEADERS and CONTINUATION frames.
        -- TODO: As it stands now, the server will happily start a new stream with
        -- a CONTINUATION frame instead of a HEADERS frame. That's against the
        -- protocol.
        MiddleFrame_SIC frame | Just (stream_id, bytes) <- isAboutHeaders frame -> do
            -- Just append the frames to streamRequestHeaders
            opens_stream <- appendHeaderFragmentBlock stream_id bytes

            if opens_stream
              then do
                maybe_rcv_headers_of <- liftIO $ takeMVar receiving_headers_mvar
                case maybe_rcv_headers_of of
                  Just _ -> do
                    -- Bad client, it is already sending headers
                    -- and trying to open another one
                    closeConnectionBecauseIsInvalid NH2.ProtocolError
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
                        INSTRUMENTATION( errorM "HTTP2.Session" "Protocol error: bad stream id")
                        closeConnectionBecauseIsInvalid NH2.ProtocolError
              else do
                maybe_rcv_headers_of <- liftIO $ takeMVar receiving_headers_mvar
                case maybe_rcv_headers_of of
                    Just a_stream_id | a_stream_id == stream_id -> do
                        -- Nothing to complain about
                        liftIO $ putMVar receiving_headers_mvar maybe_rcv_headers_of

                    Nothing -> error "InternalError, this should be set"

            if frameEndsHeaders frame then
              do
                -- Mark this time DEBUG
                liftIO $ logit $ "headers-received " `mappend` (pack . show $ stream_id )
                -- /DEBUG
                -- Ok, let it be known that we are not receiving more headers
                liftIO $ modifyMVar_
                    receiving_headers_mvar
                    (\ _ -> return Nothing )
                -- Let's decode the headers
                let for_worker_thread     = set streamId stream_id for_worker_thread_uns
                headers_bytes             <- getHeaderBytes stream_id
                dyn_table                 <- liftIO $ takeMVar decode_headers_table_mvar
                (new_table, header_list ) <- liftIO $ HP.decodeHeader dyn_table headers_bytes
                -- DEBUG
                let
                    (Just path) = He.fetchHeader header_list ":path"
                liftIO $ logit $ (pack . show $ stream_id ) `mappend` " -> " `mappend` path
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

                maybe_good_headers_editor <- validateIncomingHeaders headers_editor

                good_headers <- case maybe_good_headers_editor of
                    Just yes_they_are_good -> return yes_they_are_good
                    Nothing -> closeConnectionBecauseIsInvalid NH2.ProtocolError

                -- Add any extra headers, on demand
                headers_extra_good      <- addExtraHeaders good_headers
                let
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

                liftIO $ logit $ "headers-received-2 " `mappend` (pack . show $ stream_id )

                -- TODO: Handle the cases where a request tries to send data
                -- even if the method doesn't allow for data.

                -- I'm clear to start the worker, in its own thread
                --
                -- NOTE: Some late internal errors from the worker thread are
                --       handled here by clossing the session.
                --
                -- TODO: Log exceptions handled here.
                liftIO $ do
                    thread_id <- forkIO $ E.catch
                        (runReaderT
                            (workerThread (header_list_after, post_data_source) coherent_worker)
                            for_worker_thread
                        )
                        (
                            (   \ _ ->  do
                                -- Actions to take when the thread breaks....
                                writeChan session_input InternalAbort_SIC
                            )
                            :: HTTP500PrecursorException -> IO ()
                        )

                    H.insert stream2workerthread stream_id thread_id

                return ()
            else
                -- Frame doesn't end the headers... it was added before... so
                -- probably do nothing
                return ()

            continue

        MiddleFrame_SIC frame@(NH2.Frame _ (NH2.RSTStreamFrame _error_code_id)) -> do
            let stream_id = streamIdFromFrame frame
            liftIO $ do
                INSTRUMENTATION( infoM "HTTP2.Session" $ "Stream reset: " ++ (show _error_code_id) )
                cancelled_streams <- takeMVar cancelled_streams_mvar
                INSTRUMENTATION( infoM "HTTP2.Session" $ "Cancelled stream was: " ++ (show stream_id) )
                putMVar cancelled_streams_mvar $ NS.insert  stream_id cancelled_streams
                maybe_thread_id <- H.lookup stream2workerthread stream_id
                case maybe_thread_id  of
                    Nothing ->
                        -- This is actually more like an internal error, when this
                        -- happens, cancel the session
                        error "InterruptingUnexistentStream"

                    Just thread_id -> do
                        INSTRUMENTATION( infoM "HTTP2.Session" $ "Stream successfully interrupted" )
                        throwTo thread_id StreamCancelledException

            continue

        MiddleFrame_SIC frame@(NH2.Frame (NH2.FrameHeader _ _ nh2_stream_id) (NH2.DataFrame somebytes))
          -> unlessReceivingHeaders $ do
            -- So I got data to process
            -- TODO: Handle end of stream
            let stream_id = NH2.fromStreamIdentifier nh2_stream_id
            -- TODO: Handle the cases where the stream_id doesn't match an already existent
            -- stream. In such cases it is justified to reset the connection with a  protocol_error.

            streamWorkerSendData stream_id somebytes
            -- After that data has been received and forwarded downstream, we can issue a windows update
            --
            -- TODO: We can use wider frames to avoid congestion...
            -- .... and we can also be more compositional with these short bursts of data....
            --
            -- TODO: Consider that the best place to output these frames can be somewhere else...
            --
            -- TODO: Use a special, with-quota queue here to do flow control. Don't send meaningless
            --       WindowUpdateFrame's
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

        MiddleFrame_SIC (NH2.Frame (NH2.FrameHeader _ flags _) (NH2.PingFrame _)) | NH2.testAck flags-> do
            -- Deal with pings: this is an Ack, so do nothing
            continue

        MiddleFrame_SIC (NH2.Frame (NH2.FrameHeader _ _ _) (NH2.PingFrame somebytes))  -> do
            -- Deal with pings: NOT an Ack, so answer
            INSTRUMENTATION( debugM "HTTP2.Session" "Ping processed" )
            sendOutFrame
                (NH2.EncodeInfo
                    (NH2.setAck NH2.defaultFlags)
                    (NH2.toStreamIdentifier 0)
                    Nothing
                )
                (NH2.PingFrame somebytes)

            continue

        MiddleFrame_SIC (NH2.Frame frame_header (NH2.SettingsFrame _)) | isSettingsAck frame_header -> do
            -- Frame was received by the peer, do nothing here...
            continue

        -- TODO: Do something with these settings!!
        MiddleFrame_SIC (NH2.Frame _ (NH2.SettingsFrame settings_list))  -> do
            INSTRUMENTATION( debugM "HTTP2.Session" $ "Received settings: " ++ (show settings_list) )
            -- Just acknowledge the frame.... for now
            handleSettingsFrame settings_list
            continue

        MiddleFrame_SIC somethingelse ->  unlessReceivingHeaders $ do
            -- An undhandled case here....
            INSTRUMENTATION( errorM "HTTP2.Session" $  "Received problematic frame: " )
            INSTRUMENTATION( errorM "HTTP2.Session" $  "..  " ++ (show somethingelse) )

            continue

  where
    continue = sessionInputThread

    -- TODO: Do use the settings!!!
    handleSettingsFrame :: NH2.SettingsList -> ReaderT SessionData IO ()
    handleSettingsFrame _settings_list =
        sendOutFrame
            (NH2.EncodeInfo
                (NH2.setAck NH2.defaultFlags)
                (NH2.toStreamIdentifier 0)
                Nothing )
            (NH2.SettingsFrame [])




sendOutFrame :: NH2.EncodeInfo -> NH2.FramePayload -> ReaderT SessionData IO ()
sendOutFrame encode_info payload = do
    session_output_mvar <- view sessionOutput

    session_output <- liftIO $ takeMVar session_output_mvar
    liftIO $ writeChan session_output $ Right (encode_info, payload)
    liftIO $ putMVar session_output_mvar session_output


-- TODO: This function, but using the headers editor, triggers
--       some renormalization of the header order. A good thing, if
--       I get that order well enough....
addExtraHeaders :: He.HeaderEditor -> ReaderT SessionData IO He.HeaderEditor
addExtraHeaders headers_editor = do
    let
        enriched_lens = (sessionsContext . sessionsConfig .sessionsEnrichedHeaders )
        -- TODO: Figure out which is the best way to put this contact in the
        --       source code
        protocol_lens = He.headerLens "second-transfer-eh--used-protocol"

    add_used_protocol <- view (enriched_lens . addUsedProtocol )

    -- liftIO $ putStrLn $ "AAA" ++ (show add_used_protocol)

    let
        he1 = if add_used_protocol
            then set protocol_lens (Just "HTTP/2") headers_editor
            else headers_editor

    if add_used_protocol
        -- Nothing will be computed here if the headers are not modified.
        then return he1
        else return headers_editor


validateIncomingHeaders :: He.HeaderEditor -> ReaderT SessionData IO (Maybe He.HeaderEditor)
validateIncomingHeaders headers_editor = do
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
        (isJust maybe_path )
        then
            return (Just h1)
        else
            return Nothing


-- Sends a GO_AWAY frame and raises an exception, effectively terminating the input
-- thread of the session.
closeConnectionBecauseIsInvalid :: NH2.ErrorCodeId -> ReaderT SessionData IO a
closeConnectionBecauseIsInvalid error_code = do
    -- liftIO $ errorM "HTTP2.Session" "closeConnectionBecauseIsInvalid called!"
    last_good_stream_mvar <- view lastGoodStream
    last_good_stream <- liftIO $ takeMVar last_good_stream_mvar
    session_output_mvar <- view sessionOutput
    stream2workerthread <- view stream2WorkerThread
    sendOutFrame
        (NH2.EncodeInfo
            NH2.defaultFlags
            (NH2.toStreamIdentifier 0)
            Nothing
        )
        (NH2.GoAwayFrame
            (NH2.toStreamIdentifier last_good_stream)
            error_code
            ""
        )

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
        writeChan session_output $ Left CancelSession_SOC
        putMVar session_output_mvar session_output

        -- And unwind the input thread in the session, so that the
        -- exception handler runs....
        E.throw HTTP2ProtocolException


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
      then
        -- So, this frame is highly illegal
        closeConnectionBecauseIsInvalid NH2.ProtocolError
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
            liftIO $ putMVar chan Nothing

        Nothing ->
            -- TODO: This is a protocol error, handle it properly
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
            -- created when the headers end (and if the headers
            -- do not finish the stream)
            error "Internal error"


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


sendPrimitive500Error :: IO PrincipalStream
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



workerThread :: Request -> CoherentWorker -> WorkerMonad ()
workerThread req coherent_worker =
  do
    headers_output <- view headersOutput
    stream_id      <- view streamId

    -- TODO: Handle exceptions here: what happens if the coherent worker
    --       throws an exception signaling that the request is ill-formed
    --       and should be dropped? That could happen in a couple of occassions,
    --       but really most cases should be handled here in this file...
    (headers, _, data_and_conclussion) <-
        liftIO $ E.catch
            ( do
                (h, x, d) <- coherent_worker req
                return $! (h,x,d)
            )
            (
                (\ _ -> sendPrimitive500Error )
                :: HTTP500PrecursorException -> IO (Headers, PushedStreams, DataAndConclusion)
            )

    -- Now I send the headers, if that's possible at all
    headers_sent <- liftIO $ newEmptyMVar
    liftIO $ writeChan headers_output (stream_id, headers_sent, headers)

    -- At this moment I should ask if the stream hasn't been cancelled by the browser before
    -- commiting to the work of sending addtitional data... this is important for pushed
    -- streams
    is_stream_cancelled <- isStreamCancelled stream_id
    if not is_stream_cancelled

      then do
        -- I have a beautiful source that I can de-construct...
        -- TODO: Optionally pulling data out from a Conduit ....
        -- liftIO ( data_and_conclussion $$ (_sendDataOfStream stream_id) )
        --
        -- This threadlet should block here waiting for the headers to finish going
        -- NOTE: Exceptions generated here inheriting from HTTP500PrecursorException
        -- are let to bubble and managed in this thread fork point...
        (_maybe_footers, _) <- runConduit $
            (transPipe liftIO data_and_conclussion)
            `fuseBothMaybe`
            (sendDataOfStream stream_id headers_sent)
        -- BIG TODO: Send the footers ... likely stream conclusion semantics
        -- will need to be changed.
        return ()
      else

        return ()

--                                                       v-- comp. monad.
sendDataOfStream :: GlobalStreamId -> MVar HeadersSent -> Sink B.ByteString (ReaderT WorkerThreadEnvironment IO) ()
sendDataOfStream stream_id headers_sent = do
    data_output <- view dataOutput
    -- Wait for all headers sent
    liftIO $ takeMVar headers_sent
    consumer data_output
  where
    consumer data_output = do
        maybe_bytes <- await
        case maybe_bytes of
            Nothing ->
                liftIO $ putMVar data_output (stream_id, Nothing)
            Just bytes -> do
                liftIO $ putMVar data_output (stream_id, Just bytes)
                consumer data_output


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
    = Just (NH2.fromStreamIdentifier stream_id, block_fragment)
isAboutHeaders (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.ContinuationFrame block_fragment) )
    = Just (NH2.fromStreamIdentifier stream_id, block_fragment)
isAboutHeaders _
    = Nothing


frameEndsHeaders  :: InputFrame -> Bool
frameEndsHeaders (NH2.Frame (NH2.FrameHeader _ flags _) _) = NH2.testEndHeader flags


streamIdFromFrame :: InputFrame -> GlobalStreamId
streamIdFromFrame (NH2.Frame (NH2.FrameHeader _ _ stream_id) _) = NH2.fromStreamIdentifier stream_id


-- TODO: Have different size for the headers..... just now going with a default size of 16 k...
-- TODO: Find a way to kill this thread....
headersOutputThread :: Chan (GlobalStreamId, MVar HeadersSent, Headers)
                       -> MVar SessionOutputChannelAbstraction
                       -> ReaderT SessionData IO ()
headersOutputThread input_chan session_output_mvar = forever $ do
    (stream_id, headers_ready_mvar, headers) <- liftIO $ readChan input_chan

    -- First encode the headers using the table
    encode_dyn_table_mvar <- view toEncodeHeaders

    encode_dyn_table <- liftIO $ takeMVar encode_dyn_table_mvar
    (new_dyn_table, data_to_send ) <- liftIO $ HP.encodeHeader HP.defaultEncodeStrategy encode_dyn_table headers
    liftIO $ putMVar encode_dyn_table_mvar new_dyn_table

    -- Now split the bytestring in chunks of the needed size....
    bs_chunks <- return $! bytestringChunk useChunkLength data_to_send

    -- And send the chunks through while locking the output place....
    liftIO $ E.bracket
        (takeMVar session_output_mvar)
        (putMVar session_output_mvar )
        (\ session_output -> do
            writeIndividualHeaderFrames session_output stream_id bs_chunks True
            -- And say that the headers for this thread are out
            -- INSTRUMENTATION( debugM "HTTP2.Session" $ "Headers were output for stream " ++ (show stream_id) )
            putMVar headers_ready_mvar HeadersSent
            )
  where
    writeIndividualHeaderFrames ::
        SessionOutputChannelAbstraction
        -> GlobalStreamId
        -> [B.ByteString]
        -> Bool
        -> IO ()
    writeIndividualHeaderFrames session_output stream_id (last_fragment:[]) is_first =
        writeChan session_output $ Right ( NH2.EncodeInfo {
            NH2.encodeFlags     = NH2.setEndHeader NH2.defaultFlags
            ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id
            ,NH2.encodePadding  = Nothing },
            (if is_first then NH2.HeadersFrame Nothing last_fragment else  NH2.ContinuationFrame last_fragment)
            )
    writeIndividualHeaderFrames session_output stream_id  (fragment:xs) is_first = do
        writeChan session_output $ Right ( NH2.EncodeInfo {
            NH2.encodeFlags     = NH2.defaultFlags
            ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id
            ,NH2.encodePadding  = Nothing },
            (if is_first then NH2.HeadersFrame Nothing fragment else  NH2.ContinuationFrame fragment)
            )
        writeIndividualHeaderFrames session_output stream_id xs False


bytestringChunk :: Int -> B.ByteString -> [B.ByteString]
bytestringChunk len s | (B.length s) < len = [ s ]
bytestringChunk len s = h:(bytestringChunk len xs)
  where
    (h, xs) = B.splitAt len s


-- TODO: find a clean way to finish this thread (maybe with negative stream ids?)
-- TODO: This function does non-optimal chunking for the case where responses are
--       actually streamed.... in those cases we need to keep state for frames in
--       some other format....
-- TODO: Right now, we are transmitting an empty last frame with the end-of-stream
--       flag set. I'm afraid that the only
--       way to avoid that is by holding a frame or by augmenting the end-user interface
--       so that the user can signal which one is the last frame. The first approach
--       restricts responsiviness, the second one clutters things.
dataOutputThread :: MVar DataOutputToConveyor
                    -> MVar SessionOutputChannelAbstraction
                    -> IO ()
dataOutputThread input_chan session_output_mvar = forever $ do
    (stream_id, maybe_contents) <- takeMVar input_chan
    case maybe_contents of
        Nothing -> do
            liftIO $ do
                withLockedSessionOutput
                    (\ session_output ->    writeChan session_output $ Right ( NH2.EncodeInfo {
                             NH2.encodeFlags     = NH2.setEndStream NH2.defaultFlags
                            ,NH2.encodeStreamId  = NH2.toStreamIdentifier stream_id
                            ,NH2.encodePadding   = Nothing },
                            NH2.DataFrame ""
                            )
                        )

        Just contents -> do
            -- And now just simply output it...
            let bs_chunks = bytestringChunk useChunkLength $! contents
            -- And send the chunks through while locking the output place....
            writeContinuations bs_chunks stream_id

  where

    withLockedSessionOutput = E.bracket
        (takeMVar session_output_mvar)
        (putMVar session_output_mvar) -- <-- There is an implicit argument there!!

    writeContinuations :: [B.ByteString] -> GlobalStreamId  -> IO ()
    writeContinuations fragments stream_id  = mapM_ (\ fragment ->
        withLockedSessionOutput (\ session_output -> writeChan session_output $ Right ( NH2.EncodeInfo {
            NH2.encodeFlags     = NH2.defaultFlags
            ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id
            ,NH2.encodePadding  = Nothing },
            NH2.DataFrame fragment ) )
        ) fragments
