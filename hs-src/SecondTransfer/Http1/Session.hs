{-# LANGUAGE OverloadedStrings, RankNTypes, TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http1.Session(
    http11Attendant
    ) where


import           Control.Lens
import           Control.Exception                       (
                                                          catch,
                                                          try,
                                                          catches,
                                                          toException,
                                                          Handler      (..),
                                                          SomeException,
                                                          AsyncException
                                                         )
import           Control.Concurrent                      (newMVar, MVar, readMVar, modifyMVar_)
import           Control.Monad.IO.Class                  (liftIO, MonadIO)
import           Control.Monad                           (when)
import qualified Control.Monad.Trans.Resource            as ReT
import           Control.Monad.Morph                     (hoist, lift)
import           Control.Monad.Trans.Reader

import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                    as LB
import qualified Data.ByteString.Builder                 as Bu
import           Data.Conduit
import qualified Data.Conduit.List                       as CL
import           Data.IORef
import qualified Data.Attoparsec.ByteString              as Ap
import qualified Data.Map.Strict                         as Dm

import           SimpleHttpHeadersHq

import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.Sessions.Internal        (SessionsContext,
                                                          sessionsConfig)

import           SecondTransfer.IOCallbacks.Types

-- And we need the time
import           System.Clock

import           SecondTransfer.Http1.Parse
import           SecondTransfer.Exception                (
                                                         IOProblem,
                                                         NoMoreDataException,
                                                         HTTP11SyntaxException  (..),
                                                         ForwardedGatewayException,
                                                         forkIOExc,
                                                         traceIOExc
                                                         )
import           SecondTransfer.Sessions.ActivityMonitor
import           SecondTransfer.Sessions.Config


-- import           Debug.Trace                             (traceShow)

-- | Used to control an HTTP/1.1 session. Mostly contains data about session
--   activity
data SimpleSessionMetrics  = SimpleSessionMetrics {
  -- | The activity monitor
    _activityMonitor_SSM :: ActivityMonitor
  -- | The shared sessions context, that we use to among other
  --   things assign labels to sessions and get other read-only configuration
  --   bits.
  , _sessionsContext_SSM :: SessionsContext
  -- | To be called when closing the connection
  , _closeAction_SSM     :: IO ()
  -- | To signal that a session is ending and that no more reads should be made
  , _sessionIsEnding_SSM :: MVar Bool
  -- | The concrete session number we assigned to this session
  , _sessionTag_SSM      :: !ConnectionId
  -- | And the session store
  , _sessionStore_SSM    :: MVar SessionStore
  }

makeLenses ''SimpleSessionMetrics


type Http1Session = ReaderT SimpleSessionMetrics IO


instance ActivityMeteredSession SimpleSessionMetrics where
    sessionLastActivity ssm  = getLastActivity (ssm ^. activityMonitor_SSM)


instance HasConnectionId SimpleSessionMetrics where
    getConnectionId ssm = ssm ^. sessionTag_SSM


instance CleanlyPrunableSession SimpleSessionMetrics where
    cleanlyCloseSession s = (flip runReaderT) s $ do
        -- Start by signaling that we are not
        -- doing much more
        session_is_ending_mvar <- view sessionIsEnding_SSM
        liftIO $ modifyMVar_ session_is_ending_mvar $ \ _ -> return True
        -- Now let's wait for idle time
        activity_monitor <- view activityMonitor_SSM
        timeouts <- view (sessionsContext_SSM . sessionsConfig . sessionCloseTimeouts)
        close_action <- view closeAction_SSM
        _wait_interrupted_reason <- liftIO $ waitForIdle
            activity_monitor
            (timeouts ^. smallWait_SCT)
            (timeouts ^. maxWait_SCT)
        liftIO $ close_action
        maybe_situation_callback <- view ( sessionsContext_SSM . sessionsConfig . sessionsCallbacks .  situationCallback_SC)
        conn_id <- view sessionTag_SSM
        case maybe_situation_callback of
            Just sc -> liftIO $ sc conn_id PreemptingConnection_SWC
            Nothing -> return ()
        return ()



-- | Session attendant that speaks HTTP/1.1
--
-- This attendant should be OK with keep-alive, but not with pipelining.
-- Notice that the attendant works in its own thread, so a return from here
-- is no warranty that we are done.
--
-- Also,  that  a content-length header must be set in any
-- answer that carries a body (because of HTTP status and HTTP method of
-- the request), or otherwise the session will add a 'transfer-encoding: chunked'
-- header and send the response as chunked.
--
http11Attendant :: SessionsContext -> AwareWorker -> Attendant
http11Attendant sessions_context coherent_worker connection_info attendant_callbacks
    =
    do
        not_ending <- newMVar False
        -- started_time <- getTime Monotonic
        activity_monitor <- newActivityMonitor
        _ <- forkIOExc "Http1Go" $ do

            -- A session store needs to be passed to the coherent worker. For HTTP/2, it makes sense
            -- to have some server-set data shared by all streams in the same connection. For HTTP/1.1,
            -- a connection is quite an isolated thing, and the session stored is mostly useless.
            -- Nevertheless, one is still required for the CoherentWorker.
            the_session_store <- newMVar Dm.empty

            let
               connection_id = connection_info ^. connId_CnD
               handle = SimpleSessionMetrics {
                  _activityMonitor_SSM = activity_monitor,
                  _sessionsContext_SSM = sessions_context,
                  _closeAction_SSM     = close_action,
                  _sessionIsEnding_SSM = not_ending,
                  _sessionTag_SSM      = connection_id,
                  _sessionStore_SSM    = the_session_store
                 }
            case maybe_hashable_addr of
                 Just hashable_addr ->
                     new_session
                        hashable_addr
                        (Whole_SGH handle)
                        push_action
                        connection_id

                 Nothing ->
                     -- putStrLn "Warning, created session without registering it"
                     return ()

            catches
                (runReaderT (go (Just "") 1) handle)
                [
                  Handler (http1_error_handler connection_id),
                  Handler (http1gw_error_handler connection_id)
                ]

        return ()
  where
    maybe_hashable_addr = connection_info ^. addr_CnD

    push_action = attendant_callbacks ^. pushAction_IOC
    -- pull_action = attendant_callbacks ^. pullAction_IOC
    close_action = attendant_callbacks ^. closeAction_IOC
    best_effort_pull_action = attendant_callbacks ^. bestEffortPullAction_IOC
    close_action_called_mvar = attendant_callbacks ^. closeActionCalled_IOC

    http1_error_handler :: Monoid a => ConnectionId ->  HTTP11SyntaxException -> IO a
    http1_error_handler conn_id exc =
      do
        let
          maybe_error_callback = sessions_context ^. sessionsConfig . sessionsCallbacks .  reportErrorCallback_SC
        case maybe_error_callback of
          Nothing -> do
              putStrLn $ "There was an HTTP11 error, but no report callback was specified: " ++ show exc
              return mempty
          Just err_callback -> do
              err_callback (FrontendHTTP1SyntaxError_SWC, SessionCoordinates conn_id, toException exc)
              return mempty

    -- Used when we get invalid HTTP/1.1 from the gateway.
    http1gw_error_handler :: Monoid a => ConnectionId ->  ForwardedGatewayException -> IO a
    http1gw_error_handler conn_id exc =
      do
        let
          maybe_error_callback = sessions_context ^. sessionsConfig . sessionsCallbacks .  reportErrorCallback_SC
        case maybe_error_callback of
          Nothing -> do
              putStrLn $ "There was an HTTP11 error, but no report callback was specified: " ++ show exc
              return mempty
          Just err_callback -> do
              err_callback (BackendHTTP1SyntaxError_SWC, SessionCoordinates conn_id, toException exc)
              return mempty

    -- Registers the new session with the upper layer, via a callback, if that
    -- callback exists.
    -- At the moment, it also set a short time-to-live for the connection.
    new_session :: HashableSockAddr -> SessionGenericHandle -> forall a . a -> ConnectionId ->  IO ()
    new_session address generic_handle _weakable_key conn_id =
      do
        case maybe_callback of
          Just (NewSessionCallback callback) ->
              callback
                  address
                  generic_handle
                  close_action_called_mvar
          Nothing -> return ()
        case maybe_set_soft_hard_close of
          Just callback -> do
              callback conn_id DropIt_ETD
          Nothing -> return ()
      where
        maybe_callback =
            (sessions_context ^.
                 (sessionsConfig . sessionsCallbacks . newSessionCallback_SC)
            )
        maybe_set_soft_hard_close =
            (sessions_context ^.
                 (sessionsConfig . sessionsCallbacks . setSoftHardClose_SC)
            )

    -- Manages a full request-response cycle.
    go ::  Maybe LB.ByteString -> Int -> Http1Session ()
    go (Just leftovers) reuse_no  = do
        session_tag <- view sessionTag_SSM
        the_session_store <- view sessionStore_SSM
        maybe_leftovers <-
            add_data
                newIncrementalHttp1Parser
                leftovers
                reuse_no
        session_is_ending_mvar <- view sessionIsEnding_SSM
        session_is_ending <- liftIO $ readMVar session_is_ending_mvar
        if not session_is_ending
          then
            go  maybe_leftovers (reuse_no + 1)
          else
            -- If session has been marked as ending, do not receive any more data here, but just let the
            -- connection be closed by the upper layer.
            return ()

    go Nothing _ =
        return ()

    -- This function will invoke itself as long as data is coming for the currently-being-parsed
    -- request/response. When done, it will return Nothing to close the current session (and connection)
    -- or Just "" to continue handling requests and responses in the current connection.
    add_data ::
        IncrementalHttp1Parser  ->
        LB.ByteString ->
        Int ->
        Http1Session (Maybe LB.ByteString)
    add_data parser bytes reuse_no = do
        session_tag <- view sessionTag_SSM
        the_session_store <- view sessionStore_SSM
        sessions_context <- view sessionsContext_SSM
        activity_monitor <- view activityMonitor_SSM
        let
            completion = addBytes parser bytes

        -- We received a few bytes, in one direction or another. Report such an event.
        liftIO $ reportEvent activity_monitor
        case completion of

            RequestIsMalformed_H1PC _msg -> do
                --putStrLn $ "Syntax Error: " ++ msg
                -- This is a syntactic error..., so just close the connection
                liftIO $ close_action
                -- We exit by returning nothing
                return Nothing

            MustContinue_H1PC new_parser -> do
                maybe_new_bytes <- liftIO $ catches
                    (do
                        -- Try to get at least 16 bytes. For HTTP/1 requests, that may not be always
                        -- possible
                        new_bytes <- best_effort_pull_action True
                        return $ Just new_bytes
                    )
                    [
                        Handler ( (\ _e -> do
                             -- This is a pretty harmless condition that happens
                             -- often when the remote peer closes the connection
                             close_action
                             return Nothing
                         ) :: IOProblem -> IO (Maybe LB.ByteString) ),

                        Handler ( (\ _e -> do
                             -- This happens when we kill the processing thread
                             -- because E.G. the transfer is going too slowly
                             close_action
                             return Nothing
                         ) :: AsyncException -> IO (Maybe LB.ByteString) )

                    ]
                case maybe_new_bytes of
                    Just new_bytes ->
                        add_data
                            new_parser
                            new_bytes
                            reuse_no
                    Nothing -> return Nothing

            OnlyHeaders_H1PC headers _leftovers -> do
                -- putStrLn $ "OnlyHeaders_H1PC " ++ (show _leftovers)
                -- Ready for action...
                -- ATTENTION: Not use for pushed streams here....
                -- We must decide what to do if the user return those
                -- anyway.
                let
                    modified_headers = addExtraHeaders sessions_context headers
                    (ConnectionId conn_id_int) = session_tag
                liftIO $ do
                    started_time <-  getTime Monotonic
                    --(response_headers, _, data_and_conclusion)
                    principal_stream <- coherent_worker Request {
                            _headers_RQ = modified_headers,
                            _inputData_RQ = Nothing,
                            _perception_RQ = Perception {
                              _startedTime_Pr            = started_time,
                              _streamId_Pr               = reuse_no,
                              _sessionId_Pr              = fromIntegral conn_id_int,
                              _protocol_Pr               = Http11_HPV,
                              _anouncedProtocols_Pr      = Nothing,
                              _peerAddress_Pr            = maybe_hashable_addr,
                              _pushIsEnabled_Pr          = False,
                              _sessionLatencyRegister_Pr = [],
                              _sessionStore_Pr           = the_session_store,
                              _perceivedPriority_Pr      = Nothing
                            }
                        }
                    ReT.runResourceT $
                        answer_by_principal_stream
                            principal_stream
                            activity_monitor
                    -- Will discard leftovers, but can continue
                    return $ Just ""

            -- We close the connection if any of the delimiting headers could not be parsed.
            HeadersAndBody_H1PC _headers SemanticAbort_BSC _recv_leftovers -> do
                liftIO $ close_action
                return Nothing

            HeadersAndBody_H1PC headers stopcondition recv_leftovers -> do
                let
                    modified_headers = addExtraHeaders sessions_context headers
                    (ConnectionId conn_id_int) = session_tag
                liftIO $ do
                    started_time <- getTime Monotonic
                    set_leftovers <- newIORef ""

                    -- putStrLn $ "STOP condition: " ++ (show stopcondition)

                    let
                        source :: Source AwareWorkerStack B.ByteString
                        source = hoist  lift $ case stopcondition of
                            UseBodyLength_BSC n -> counting_read (LB.fromStrict recv_leftovers) n set_leftovers
                            ConnectionClosedByPeer_BSC -> readforever (LB.fromStrict recv_leftovers)
                            Chunked_BSC -> readchunks (LB.fromStrict recv_leftovers)
                            _ -> error "HeadersAndBodyImplementMe"

                    principal_stream <- coherent_worker Request {
                            _headers_RQ = modified_headers,
                            _inputData_RQ = Just source,
                            _perception_RQ = Perception {
                              _startedTime_Pr            = started_time,
                              _streamId_Pr               = reuse_no,
                              _sessionId_Pr              = fromIntegral conn_id_int,
                              _protocol_Pr               = Http11_HPV,
                              _anouncedProtocols_Pr      = Nothing,
                              _peerAddress_Pr            = maybe_hashable_addr,
                              _pushIsEnabled_Pr          = False,
                              _sessionLatencyRegister_Pr = [],
                              _sessionStore_Pr           = the_session_store,
                              _perceivedPriority_Pr      = Nothing
                            }
                        }
                    ReT.runResourceT $
                        answer_by_principal_stream
                           principal_stream
                           activity_monitor
                    -- Will discard left-overs, but can continue handling requests and responses in the current
                    -- connection.
                    return $ Just ""

    counting_read :: LB.ByteString -> Int -> IORef LB.ByteString -> Source IO B.ByteString
    counting_read leftovers n set_leftovers = do
        -- Can I continue?
        if n == 0
          then do
            liftIO $ writeIORef set_leftovers  leftovers
            return ()
          else
            do
              let
                  lngh_leftovers = LB.length leftovers
                  n64 = fromIntegral n
              if lngh_leftovers > 0
                then
                  if lngh_leftovers <= n64
                    then do
                      -- yield leftovers
                      CL.sourceList (LB.toChunks leftovers)
                      counting_read "" (fromIntegral $ n64 - lngh_leftovers ) set_leftovers
                    else
                      do
                        let
                          (pass, new_leftovers) = LB.splitAt n64 leftovers
                        CL.sourceList (LB.toChunks pass)
                        counting_read new_leftovers  0 set_leftovers
                else
                  do
                    more_text <- liftIO $ best_effort_pull_action True
                    counting_read more_text n set_leftovers

    readforever :: LB.ByteString  -> Source IO B.ByteString
    readforever leftovers  =
        if LB.length leftovers > 0
          then do
            CL.sourceList (LB.toChunks  leftovers)
            readforever mempty
          else do
            more_text_or_error <- liftIO . try $ best_effort_pull_action True
            case more_text_or_error :: Either NoMoreDataException LB.ByteString of
                Left _ -> return ()
                Right bs -> do
                    when (LB.length bs > 0) $
                       CL.sourceList (LB.toChunks bs)
                    readforever mempty

    readchunks :: LB.ByteString -> Source IO B.ByteString
    readchunks leftovers = do
        let
            gorc :: LB.ByteString ->
                    (B.ByteString -> Ap.IResult B.ByteString B.ByteString ) ->
                    Source IO B.ByteString
            gorc lo f = case f (LB.toStrict lo) of
                Ap.Fail _ _ _ ->
                  return ()
                Ap.Partial continuation ->
                  do
                    more_text_or_error <- liftIO . try $ best_effort_pull_action True
                    case more_text_or_error :: Either NoMoreDataException LB.ByteString of
                        -- Shouldn't I raise an exception here?
                        Left _ -> return ()
                        Right bs
                          | LB.length bs > 0 -> gorc bs continuation
                          | otherwise -> return ()
                Ap.Done i piece ->
                  do
                    if B.length piece > 0
                       then do
                           --liftIO $ putStrLn "SawContentRichPiece"
                           yield piece
                           gorc (LB.fromStrict i) (Ap.parse chunkParser)
                       else do
                           -- liftIO $ putStrLn "SawEmptyPiece"
                           return ()
        gorc leftovers (Ap.parse chunkParser)

    maybepushtext :: MonadIO m => LB.ByteString -> m Bool
    maybepushtext txt =  liftIO $
        catch
            (do
                push_action txt
                return True
            )
            ((\ _e -> do
                -- debugM "Session.HTTP1" "Session abandoned"
                close_action
                return False
            ) :: IOProblem -> IO Bool )

    piecewiseconsume :: MonadIO m => Sink LB.ByteString m Bool
    piecewiseconsume = do
        maybe_text <- await
        case maybe_text of
            Just txt -> do
                can_continue <- liftIO $ maybepushtext txt
                if can_continue
                  then
                    piecewiseconsume
                  else
                    return False

            Nothing -> return True

    piecewiseconsumecounting :: MonadIO m => Int -> Sink LB.ByteString m Bool
    piecewiseconsumecounting n
      | n > 0 = do
        maybe_text <- await
        case maybe_text of
            Just txt -> do
                let
                    send_txt = LB.take (fromIntegral n) txt
                can_continue <- liftIO $ maybepushtext send_txt
                if can_continue
                  then do
                    let
                        left_to_take = ( n - fromIntegral (LB.length send_txt))
                    piecewiseconsumecounting left_to_take
                  else
                    return False

            Nothing -> return True
      | otherwise =
            return True

    -- Sends the response to the browser, while ticking the activity monitor to detect stalled
    -- connections.
    answer_by_principal_stream :: PrincipalStream -> ActivityMonitor ->  AwareWorkerStack ()
    answer_by_principal_stream principal_stream activity_monitor = do
        let
            data_and_conclusion = principal_stream ^. dataAndConclusion_PS
            response_headers    = principal_stream ^. headers_PS
            transfer_encoding =  response_headers ^.  chunked_Hi
            cnt_length_header =  response_headers ^.  contentLength_Hi
            interrupt_effect   = principal_stream ^. effect_PS . interrupt_Ef
            status_code :: Maybe Int
            status_code =  fromIntegral <$> response_headers ^. status_Hi
            headers_text_as_lbs =
                Bu.toLazyByteString $
                    headerListToHTTP1ResponseText response_headers  `mappend` "\r\n"

        close_release_key <- ReT.register close_action

        let
            close_behavior =
                case interrupt_effect of
                    Just InterruptConnectionAfter_IEf ->
                        return () -- Let the key expire and the connection be closed
                    _ -> do
                        -- Let the connection remain open
                        _ <- ReT.unprotect close_release_key
                        return ()

            handle_as_headers_only  =
              do
                -- TODO: Take care of footers
                -- let

                liftIO $ do
                    push_action headers_text_as_lbs
                close_behavior
                return ()

            activity_reporter = do
                y <- await
                liftIO $ reportEvent activity_monitor
                case y of
                    Just yy -> do
                       yield yy
                       activity_reporter

                    Nothing -> return ()

            handle_as_chunked set_transfer_encoding =
              do
                -- TODO: Take care of footers
                let
                    headers_text_as_lbs' =
                        if set_transfer_encoding
                           then
                              Bu.toLazyByteString $
                                  (headerListToHTTP1ResponseText
                                          (set chunked_Hi True response_headers)
                                  ) `mappend` "\r\n"
                           else
                              headers_text_as_lbs
                liftIO $ do
                    reportEvent activity_monitor
                    push_action headers_text_as_lbs'
                    reportEvent activity_monitor
                -- Will run the conduit. If it fails, the connection will be closed.
                (_maybe_footers, _did_ok) <-
                    runConduit $
                        data_and_conclusion
                        `fuseBothMaybe`
                        (
                          CL.map wrapChunk
                          =$=
                          activity_reporter
                          =$=
                          piecewiseconsume
                        )
                -- Don't forget the zero-length terminating chunk...
                _ <- maybepushtext $ wrapChunk ""
                -- If I got to this point, I can keep the connection alive for a future request, unless...
                close_behavior
                return ()

        case (transfer_encoding, cnt_length_header) of

            (True, _ )  ->
                handle_as_chunked False

            (False, Just content_length) -> do
                -- Use the provided chunks, as naturally as possible
                liftIO $ push_action headers_text_as_lbs
                (_maybe_footers, _did_ok) <-
                      runConduit $
                          data_and_conclusion
                          `fuseBothMaybe`
                          (
                            CL.map LB.fromStrict
                            =$=
                            activity_reporter
                            =$=
                            piecewiseconsumecounting content_length
                          )
                -- Got here, keep the connection open if possible
                close_behavior
                return ()

            (False, Nothing)
                | Just status_no <- status_code, not (responseStatusHasResponseBody status_no) ->
                    handle_as_headers_only
                | otherwise -> do
                    -- We come here when there is no explicit transfer encoding and when there is
                    -- no content-length header. This condition should be avoided by most users of
                    -- of this library, but sometimes we are talking to an application...
                    -- There are two options here: to handle the transfer as chunked, or to close
                    -- the connection after ending...
                    handle_as_chunked True


addExtraHeaders :: SessionsContext -> HqHeaders -> HqHeaders
addExtraHeaders _sessions_context headers = headers

  -- let
  --   enriched_lens :: Lens' SessionsContext SessionsEnrichedHeaders
  --   enriched_lens = (sessionsConfig . sessionsEnrichedHeaders)
  --   -- Haskell laziness here!
  --   headers_editor = He.fromList headers
  --   -- TODO: Figure out which is the best way to put this contact in the
  --   --       source code
  --   protocol_lens = He.headerLens "second-transfer-eh--used-protocol"
  --   add_used_protocol = sessions_context ^. (enriched_lens . addUsedProtocol )
  --   he1 = if add_used_protocol
  --       then set protocol_lens (Just "HTTP/1.1") headers_editor
  --       else headers_editor
  --   result = He.toList he1

  -- in if add_used_protocol
  --       -- Nothing will be computed here if the headers are not modified.
  --       then result
  --       else headers
