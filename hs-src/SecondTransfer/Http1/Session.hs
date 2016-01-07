{-# LANGUAGE OverloadedStrings, RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http1.Session(
    http11Attendant
    ) where


import           Control.Lens
import           Control.Exception                       (catch, try)
--import           Control.Concurrent                      (forkIO)
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad                           (when)

import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                   as LB
import           Data.ByteString.Char8                  (unpack, pack)
import qualified Data.ByteString.Builder                as Bu
import           Data.Foldable                           (find)
import           Data.Conduit
import qualified Data.Conduit.List                       as CL
import           Data.IORef
import qualified Data.Attoparsec.ByteString              as Ap
-- import           Data.Monoid                            (mconcat, mappend)

import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.MainLoop.Protocol
import           SecondTransfer.Sessions.Internal        (SessionsContext, acquireNewSessionTag, sessionsConfig)

import           SecondTransfer.IOCallbacks.Types

-- And we need the time
import           System.Clock

import           SecondTransfer.Http1.Parse
import           SecondTransfer.Exception                (
                                                         IOProblem,
                                                         NoMoreDataException,
                                                         forkIOExc
                                                         )
import           SecondTransfer.Sessions.Config
import qualified SecondTransfer.Utils.HTTPHeaders        as He


-- import           Debug.Trace                             (traceShow)

-- | Used to report metrics of the session for Http/1.1
newtype SimpleSessionMetrics  = SimpleSessionMetrics TimeSpec


instance ActivityMeteredSession SimpleSessionMetrics where
    sessionLastActivity (SimpleSessionMetrics t) = return t


-- | Session attendant that speaks HTTP/1.1
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
                     putStrLn "Warning, created session without registering it"

            go started_time new_session_tag (Just "") 1
        return ()
  where
    maybe_hashable_addr = connection_info ^. addr_CnD

    push_action = attendant_callbacks ^. pushAction_IOC
    -- pull_action = attendant_callbacks ^. pullAction_IOC
    close_action = attendant_callbacks ^. closeAction_IOC
    best_effort_pull_action = attendant_callbacks ^. bestEffortPullAction_IOC

    new_session :: HashableSockAddr -> SessionGenericHandle -> forall a . a -> IO ()
    new_session a b c = case maybe_callback of
        Just (NewSessionCallback callback) -> callback a b c
        Nothing -> return ()
      where
        maybe_callback =
            (sessions_context ^. (sessionsConfig . sessionsCallbacks . newSessionCallback_SC) )

    go :: TimeSpec -> Int -> Maybe B.ByteString -> Int -> IO ()
    go started_time session_tag (Just leftovers) reuse_no = do
        maybe_leftovers <- add_data newIncrementalHttp1Parser leftovers session_tag reuse_no
        go started_time session_tag maybe_leftovers (reuse_no + 1)

    go _ _ Nothing _  =
        return ()

    -- This function will invoke itself as long as data is coming for the currently-being-parsed
    -- request/response.
    add_data :: IncrementalHttp1Parser  -> B.ByteString -> Int -> Int -> IO (Maybe B.ByteString)
    add_data parser bytes session_tag reuse_no = do
        let
            completion = addBytes parser bytes
            -- completion = addBytes parser $ traceShow ("At session " ++ (show session_tag) ++ " Received: " ++ (unpack bytes) ) bytes
        case completion of

            RequestIsMalformed_H1PC _msg -> do
                --putStrLn $ "Syntax Error: " ++ msg
                -- This is a syntactic error..., so just close the connectin
                close_action
                -- We exit by returning nothing
                return Nothing

            MustContinue_H1PC new_parser -> do
                --putStrLn "MustContinue_H1PC"
                catch
                    (do
                        -- Try to get at least 16 bytes. For HTTP/1 requests, that may not be always
                        -- possible
                        new_bytes <- best_effort_pull_action True
                        add_data new_parser new_bytes session_tag reuse_no
                    )
                    ( (\ _e -> do
                        -- This is a pretty harmless condition that happens
                        -- often when the remote peer closes the connection
                        -- debugM "Session.HTTP1" "Could not receive data"
                        close_action
                        return Nothing
                    ) :: IOProblem -> IO (Maybe B.ByteString) )

            OnlyHeaders_H1PC headers leftovers -> do
                -- putStrLn $ "OnlyHeaders_H1PC " ++ (show leftovers)
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
                          _startedTime_Pr       = started_time,
                          _streamId_Pr          = reuse_no,
                          _sessionId_Pr         = session_tag,
                          _protocol_Pr          = Http11_HPV,
                          _anouncedProtocols_Pr = Nothing,
                          _peerAddress_Pr       = maybe_hashable_addr
                        }
                    }
                answer_by_principal_stream principal_stream leftovers

            -- We close the connection of any of the delimiting headers could not be parsed.
            HeadersAndBody_H1PC _headers SemanticAbort_BSC _recv_leftovers -> do
                close_action
                return Nothing

            HeadersAndBody_H1PC headers stopcondition recv_leftovers -> do
                -- putStrLn $ "HeadersAndBody_H1PC " ++ (show recv_leftovers)
                let
                    modified_headers = addExtraHeaders sessions_context headers
                started_time <- getTime Monotonic
                set_leftovers <- newIORef ""

                let
                    source :: Source IO B.ByteString
                    source = case stopcondition of
                        UseBodyLength_BSC n -> counting_read recv_leftovers n set_leftovers
                        ConnectionClosedByPeer_BSC -> readforever recv_leftovers
                        Chunked_BSC -> readchunks recv_leftovers
                        _ -> error "ImplementMe"

                principal_stream <- coherent_worker Request {
                        _headers_RQ = modified_headers,
                        _inputData_RQ = Just source,
                        _perception_RQ = Perception {
                          _startedTime_Pr       = started_time,
                          _streamId_Pr          = reuse_no,
                          _sessionId_Pr         = session_tag,
                          _protocol_Pr          = Http11_HPV,
                          _anouncedProtocols_Pr = Nothing,
                          _peerAddress_Pr       = maybe_hashable_addr
                        }
                    }
                answer_by_principal_stream principal_stream recv_leftovers

    counting_read :: B.ByteString -> Int -> IORef B.ByteString -> Source IO B.ByteString
    counting_read leftovers n set_leftovers = do
        -- Can I continue?
        if n == 0
          then do
            liftIO $ writeIORef set_leftovers leftovers
            return ()
          else
            do
              let
                  lngh_leftovers = B.length leftovers
              if lngh_leftovers > 0
                then
                  if lngh_leftovers <= n
                    then do
                      yield leftovers
                      counting_read "" (n - lngh_leftovers ) set_leftovers
                    else
                      do
                        let
                          (pass, new_leftovers) = B.splitAt n leftovers
                        yield pass
                        counting_read new_leftovers  0 set_leftovers
                else
                  do
                    more_text <- liftIO $ best_effort_pull_action True
                    counting_read more_text n set_leftovers

    readforever :: B.ByteString  -> Source IO B.ByteString
    readforever leftovers  =
        if B.length leftovers > 0
          then do
            yield leftovers
            readforever mempty
          else do
            more_text_or_error <- liftIO . try $ best_effort_pull_action True
            case more_text_or_error :: Either NoMoreDataException B.ByteString of
                Left _ -> return ()
                Right bs -> do
                    when (B.length bs > 0) $ yield bs
                    readforever mempty

    readchunks :: B.ByteString -> Source IO B.ByteString
    readchunks leftovers = do
        let
            gorc :: B.ByteString -> (B.ByteString -> Ap.IResult B.ByteString B.ByteString ) -> Source IO B.ByteString
            gorc lo f = case f lo of
                Ap.Fail _ _ _ ->
                  return ()
                Ap.Partial continuation ->
                  do
                    more_text_or_error <- liftIO . try $ best_effort_pull_action True
                    case more_text_or_error :: Either NoMoreDataException B.ByteString of
                        Left _ -> return ()
                        Right bs
                          | B.length bs > 0 -> gorc bs continuation
                          | otherwise -> return ()
                Ap.Done i piece ->
                  do
                    if B.length piece > 0
                       then do
                           yield piece
                           gorc i (Ap.parse chunkParser)
                       else
                           return ()
        gorc leftovers (Ap.parse chunkParser)

    maybepushtext :: LB.ByteString -> IO Bool
    maybepushtext txt =  catch
        (do
            push_action txt
            return True
        )
        ((\ _e -> do
            -- debugM "Session.HTTP1" "Session abandoned"
            close_action
            return False
        ) :: IOProblem -> IO Bool )

    piecewiseconsume :: Sink LB.ByteString IO Bool
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

    piecewiseconsumecounting :: Int -> Sink LB.ByteString IO Bool
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


    answer_by_principal_stream principal_stream leftovers = do
        let
            data_and_conclusion = principal_stream ^. dataAndConclusion_PS
            response_headers    = principal_stream ^. headers_PS
            transfer_encoding = find (\ x -> (fst x) == "transfer-encoding" ) response_headers
            cnt_length_header = find (\ x -> (fst x) == "content-length" )    response_headers
            headers_text_as_lbs = Bu.toLazyByteString $ headerListToHTTP1ResponseText response_headers  `mappend` "\r\n"

        case (transfer_encoding, cnt_length_header) of

            (Just (_, enc), _ )
              | transferEncodingIsChunked enc -> do
                  -- TODO: Take care of footers
                  push_action headers_text_as_lbs
                  (_maybe_footers, did_ok) <- runConduit  $ data_and_conclusion `fuseBothMaybe` (CL.map wrapChunk =$= piecewiseconsume)
                  if did_ok
                    then do
                      -- Don't forget the zero-length terminating chunk...
                      didok' <- maybepushtext $ wrapChunk ""
                      if didok'
                        then
                          return $ Just leftovers
                        else
                          return Nothing
                    else do
                      close_action
                      return Nothing

              | otherwise -> do
                -- This is a pretty bad condition, I don't know how to use
                -- any other encoding...
                  putStrLn "ISawTransferEncodingICouldntHandle/ClosingConnection"
                  close_action
                  return Nothing

            (Nothing, (Just (_,content_length_str))) -> do
                -- Use the provided chunks, as naturally as possible
                let
                    content_length :: Int
                    content_length = read . unpack $ content_length_str
                push_action headers_text_as_lbs
                (_maybe_footers, did_ok) <- runConduit $ data_and_conclusion `fuseBothMaybe` (CL.map LB.fromStrict =$= piecewiseconsumecounting content_length)
                if did_ok
                  then
                    return $ Just leftovers
                  else do
                    close_action
                    return Nothing

            (Nothing, Nothing) -> do
                (_, fragments) <- runConduit $ fuseBoth data_and_conclusion CL.consume
                let
                    response_text =
                        Bu.toLazyByteString . mconcat . map Bu.byteString $ fragments
                    content_length_str = pack . show . LB.length $ response_text
                    h2 = He.fromList response_headers
                    h3 =
                        (set $ He.headerLens "content-length" )
                        (Just content_length_str)
                        h2
                    h4 = He.toList h3
                    headers_text_as_lbs' = Bu.toLazyByteString $ headerListToHTTP1ResponseText h4  `mappend` "\r\n"

                push_action headers_text_as_lbs'

                catch
                    (do
                        push_action response_text
                        return $ Just leftovers
                    )
                    ((\ _e -> do
                        -- debugM "Session.HTTP1" "Session abandoned"
                        close_action
                        return Nothing
                    ) :: IOProblem -> IO (Maybe B.ByteString) )

addExtraHeaders :: SessionsContext -> Headers -> Headers
addExtraHeaders sessions_context headers =
  let
    enriched_lens :: Lens' SessionsContext SessionsEnrichedHeaders
    enriched_lens = (sessionsConfig . sessionsEnrichedHeaders)
    -- Haskell laziness here!
    headers_editor = He.fromList headers
    -- TODO: Figure out which is the best way to put this contact in the
    --       source code
    protocol_lens = He.headerLens "second-transfer-eh--used-protocol"
    add_used_protocol = sessions_context ^. (enriched_lens . addUsedProtocol )
    he1 = if add_used_protocol
        then set protocol_lens (Just "HTTP/1.1") headers_editor
        else headers_editor
    result = He.toList he1

  in if add_used_protocol
        -- Nothing will be computed here if the headers are not modified.
        then result
        else headers
