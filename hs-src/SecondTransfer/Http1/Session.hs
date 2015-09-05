{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http1.Session(
    http11Attendant
    ) where


import           Control.Lens
import           Control.Exception                       (catch)
import           Control.Concurrent                      (forkIO)
import           Control.Monad.IO.Class                  (liftIO)

import qualified Data.ByteString                         as B
-- import qualified Data.ByteString.Lazy                   as LB
-- import           Data.ByteString.Char8                  (unpack)
-- import qualified Data.ByteString.Builder                as Bu
import           Data.Conduit
import           Data.Conduit.List                       (consume)
import           Data.IORef
-- import           Data.Monoid                            (mconcat, mappend)

import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.MainLoop.PushPullType
import           SecondTransfer.MainLoop.Protocol
import           SecondTransfer.Sessions.Internal        (SessionsContext, acquireNewSessionTag, sessionsConfig)

-- Logging utilities
import           System.Log.Logger
-- And we need the time
import           System.Clock

import           SecondTransfer.Http1.Parse
import           SecondTransfer.Exception                (IOProblem)
import           SecondTransfer.Sessions.Config
import qualified SecondTransfer.Utils.HTTPHeaders        as He

-- import           Debug.Trace                             (traceShow)


-- | Session attendant that speaks HTTP/1.1
--
http11Attendant :: SessionsContext -> AwareWorker -> Attendant
http11Attendant sessions_context coherent_worker attendant_callbacks
    =
    do
        new_session_tag <- acquireNewSessionTag sessions_context
        -- infoM "Session.Session_HTTP11" $ "Starting new session with tag: " ++(show new_session_tag)
        forkIO $ go new_session_tag (Just "") 1
        return ()
  where
    push_action = attendant_callbacks ^. pushAction_IOC
    -- pull_action = attendant_callbacks ^. pullAction_IOC
    close_action = attendant_callbacks ^. closeAction_IOC
    best_effort_pull_action = attendant_callbacks ^. bestEffortPullAction_IOC

    go :: Int -> Maybe B.ByteString -> Int -> IO ()
    go session_tag (Just leftovers) reuse_no = do
        -- infoM "Session.Session_HTTP11" $ "(Re)Using session with tag: " ++ (show session_tag)
        maybe_leftovers <- add_data newIncrementalHttp1Parser leftovers session_tag reuse_no
        go session_tag maybe_leftovers (reuse_no + 1)

    go _ Nothing _  =
        return ()

    -- This function will invoke itself as long as data is coming for the currently-being-parsed
    -- request/response.
    add_data :: IncrementalHttp1Parser  -> B.ByteString -> Int -> Int -> IO (Maybe B.ByteString)
    add_data parser bytes session_tag reuse_no = do
        let
            completion = addBytes parser bytes
            -- completion = addBytes parser $ traceShow ("At session " ++ (show session_tag) ++ " Received: " ++ (unpack bytes) ) bytes
        case completion of

            MustContinue_H1PC new_parser ->
                -- print "MustContinue_H1PC"
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
                -- print "OnlyHeaders_H1PC"
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
                          _anouncedProtocols_Pr = Nothing
                        }
                    }
                let
                    data_and_conclusion = principal_stream ^. dataAndConclusion_PS
                    response_headers    = principal_stream ^. headers_PS
                (_, fragments) <- runConduit $ fuseBoth data_and_conclusion consume
                let
                    response_text =
                        serializeHTTPResponse response_headers fragments

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

            HeadersAndBody_H1PC headers stopcondition recv_leftovers -> do
                let
                    modified_headers = addExtraHeaders sessions_context headers
                started_time <- getTime Monotonic
                set_leftovers <- newIORef ""

                principal_stream <- coherent_worker Request {
                        _headers_RQ = modified_headers,
                        _inputData_RQ = Just $ counting_read recv_leftovers stopcondition set_leftovers,
                        _perception_RQ = Perception {
                          _startedTime_Pr       = started_time,
                          _streamId_Pr          = reuse_no,
                          _sessionId_Pr         = session_tag,
                          _protocol_Pr          = Http11_HPV,
                          _anouncedProtocols_Pr = Nothing
                        }
                    }
                let
                    data_and_conclusion = principal_stream ^. dataAndConclusion_PS
                    response_headers    = principal_stream ^. headers_PS
                (_, fragments) <- runConduit $ fuseBoth data_and_conclusion consume
                channel_leftovers <- readIORef set_leftovers
                let
                    response_text =
                        serializeHTTPResponse response_headers fragments

                catch
                    (do
                        push_action response_text
                        return $ Just channel_leftovers
                    )
                    ((\ _e -> do
                        -- debugM "Session.HTTP1" "Session abandoned"
                        close_action
                        return Nothing
                    ) :: IOProblem -> IO (Maybe B.ByteString) )

    counting_read :: B.ByteString -> BodyStopCondition -> IORef B.ByteString -> Source IO B.ByteString
    counting_read leftovers un@(UseBodyLength_BSC n) set_leftovers = do
        -- Can I continue?
        if n == 0 then
          do
            liftIO $ writeIORef set_leftovers leftovers
            return ()
        else
          do
            let
                lngh_leftovers = B.length leftovers
            if lngh_leftovers > 0 then
                if lngh_leftovers <= n then
                  do
                    yield leftovers
                    counting_read "" (UseBodyLength_BSC (n - lngh_leftovers )) set_leftovers
                else
                  do
                    let
                      (pass, new_leftovers) = B.splitAt n leftovers
                    yield pass
                    counting_read new_leftovers (UseBodyLength_BSC 0) set_leftovers
            else
              do
                more_text <- liftIO $ best_effort_pull_action True
                counting_read more_text un set_leftovers


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
