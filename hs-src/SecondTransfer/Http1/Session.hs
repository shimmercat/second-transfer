{-# LANGUAGE OverloadedStrings, RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http1.Session(
    http11Attendant
    ) where


import           Control.Lens
import           Control.Exception                       (
                                                          catch,
                                                          try,
                                                          catches,
                                                          Handler      (..),
                                                          AsyncException
                                                         )
import           Control.Concurrent                      (newMVar, MVar)
import           Control.Monad.IO.Class                  (liftIO, MonadIO)
import           Control.Monad                           (when)
import qualified Control.Monad.Trans.Resource            as ReT
import           Control.Monad.Morph                     (hoist, lift)

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
-- import           SecondTransfer.MainLoop.Protocol
import           SecondTransfer.Sessions.Internal        (SessionsContext, acquireNewSessionTag, sessionsConfig)

import           SecondTransfer.IOCallbacks.Types

-- And we need the time
import           System.Clock

import           SecondTransfer.Http1.Parse
import           SecondTransfer.Exception                (
                                                         IOProblem,
                                                         NoMoreDataException,
                                                         -- HTTP11SyntaxException  (..),
                                                         forkIOExc
                                                         )
import           SecondTransfer.Sessions.Config


-- import           Debug.Trace                             (traceShow)

-- | Used to report metrics of the session for Http/1.1
newtype SimpleSessionMetrics  = SimpleSessionMetrics TimeSpec


instance ActivityMeteredSession SimpleSessionMetrics where
    sessionLastActivity (SimpleSessionMetrics t) = return t


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
        new_session_tag <- acquireNewSessionTag sessions_context
        started_time <- getTime Monotonic
        -- infoM "Session.Session_HTTP11" $ "Starting new session with tag: " ++(show new_session_tag)
        _ <- forkIOExc "Http1Go" $ do
            let
               handle = SimpleSessionMetrics started_time
            case maybe_hashable_addr of
                 Just hashable_addr ->
                     new_session
                        hashable_addr
                        (Partial_SGH handle attendant_callbacks)
                        push_action

                 Nothing ->
                     -- putStrLn "Warning, created session without registering it"
                     return ()

            the_session_store <- newMVar Dm.empty

            go started_time new_session_tag (Just "") 1 the_session_store
        return ()
  where
    maybe_hashable_addr = connection_info ^. addr_CnD

    push_action = attendant_callbacks ^. pushAction_IOC
    -- pull_action = attendant_callbacks ^. pullAction_IOC
    close_action = attendant_callbacks ^. closeAction_IOC
    best_effort_pull_action = attendant_callbacks ^. bestEffortPullAction_IOC
    close_action_called_mvar = attendant_callbacks ^. closeActionCalled_IOC

    new_session :: HashableSockAddr -> SessionGenericHandle -> forall a . a -> IO ()
    new_session address generic_handle weakable_key = case maybe_callback of
        Just (NewSessionCallback callback) ->
            callback
                address
                generic_handle
                close_action_called_mvar
        Nothing -> return ()
      where
        maybe_callback =
            (sessions_context ^.
                 (sessionsConfig . sessionsCallbacks . newSessionCallback_SC)
            )

    go :: TimeSpec -> Int -> Maybe LB.ByteString -> Int -> MVar SessionStore -> IO ()
    go started_time session_tag (Just leftovers) reuse_no the_session_store = do
        maybe_leftovers <-
            add_data
                newIncrementalHttp1Parser
                leftovers session_tag
                reuse_no
                the_session_store
        go started_time session_tag maybe_leftovers (reuse_no + 1) the_session_store

    go _ _ Nothing _  _ =
        return ()

    -- This function will invoke itself as long as data is coming for the currently-being-parsed
    -- request/response.
    add_data ::
        IncrementalHttp1Parser  ->
        LB.ByteString ->
        Int ->
        Int ->
        MVar SessionStore -> IO (Maybe LB.ByteString)
    add_data parser bytes session_tag reuse_no the_session_store = do
        let
            completion = addBytes parser bytes
            -- completion = addBytes parser $ traceShow ("At session " ++ (show session_tag) ++ " Received: " ++ (unpack bytes) ) bytes
        case completion of

            RequestIsMalformed_H1PC _msg -> do
                --putStrLn $ "Syntax Error: " ++ msg
                -- This is a syntactic error..., so just close the connection
                close_action
                -- We exit by returning nothing
                return Nothing

            MustContinue_H1PC new_parser -> do
                --putStrLn "MustContinue_H1PC"
                catches
                    (do
                        -- Try to get at least 16 bytes. For HTTP/1 requests, that may not be always
                        -- possible
                        new_bytes <- best_effort_pull_action True
                        add_data new_parser new_bytes session_tag reuse_no the_session_store
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

            OnlyHeaders_H1PC headers _leftovers -> do
                -- putStrLn $ "OnlyHeaders_H1PC " ++ (show _leftovers)
                -- Ready for action...
                -- ATTENTION: Not use for pushed streams here....
                -- We must decide what to do if the user return those
                -- anyway.
                let
                    modified_headers = addExtraHeaders sessions_context headers
                started_time <- getTime Monotonic
                --(response_headers, _, data_and_conclusion)
                principal_stream <- coherent_worker Request {
                        _headers_RQ = modified_headers,
                        _inputData_RQ = Nothing,
                        _perception_RQ = Perception {
                          _startedTime_Pr            = started_time,
                          _streamId_Pr               = reuse_no,
                          _sessionId_Pr              = session_tag,
                          _protocol_Pr               = Http11_HPV,
                          _anouncedProtocols_Pr      = Nothing,
                          _peerAddress_Pr            = maybe_hashable_addr,
                          _pushIsEnabled_Pr          = False,
                          _sessionLatencyRegister_Pr = [],
                          _sessionStore_Pr           = the_session_store
                        }
                    }
                ReT.runResourceT $ answer_by_principal_stream principal_stream
                -- Will discard leftovers, but can continue
                return $ Just ""

            -- We close the connection if any of the delimiting headers could not be parsed.
            HeadersAndBody_H1PC _headers SemanticAbort_BSC _recv_leftovers -> do
                close_action
                return Nothing

            HeadersAndBody_H1PC headers stopcondition recv_leftovers -> do
                let
                    modified_headers = addExtraHeaders sessions_context headers
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
                          _sessionId_Pr              = session_tag,
                          _protocol_Pr               = Http11_HPV,
                          _anouncedProtocols_Pr      = Nothing,
                          _peerAddress_Pr            = maybe_hashable_addr,
                          _pushIsEnabled_Pr          = False,
                          _sessionLatencyRegister_Pr = [],
                          _sessionStore_Pr           = the_session_store
                        }
                    }
                ReT.runResourceT $ answer_by_principal_stream principal_stream
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

    answer_by_principal_stream :: PrincipalStream ->  AwareWorkerStack ()
    answer_by_principal_stream principal_stream  = do
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
                    -- The line below causes an error which is nice to track in its way
                    -- up.
                    --putStrLn "PurpodselyCausingMyhem"
                    --push_action "\r\n"
                close_behavior
                return ()

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
                liftIO $ push_action headers_text_as_lbs'
                -- Will run the conduit. If it fails, the connection will be closed.
                (_maybe_footers, _did_ok) <-
                    runConduit $
                        data_and_conclusion
                        `fuseBothMaybe`
                        (CL.map wrapChunk =$= piecewiseconsume)
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
                          (CL.map LB.fromStrict
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
