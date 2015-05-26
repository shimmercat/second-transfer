{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Http1.Session(
    http11Attendant
    ) where 

 
import           Control.Lens
import           Control.Exception                       (catch)
import           Control.Concurrent                      (forkIO)

import qualified Data.ByteString                         as B
-- import qualified Data.ByteString.Lazy                   as LB
-- import           Data.ByteString.Char8                  (unpack)
-- import qualified Data.ByteString.Builder                as Bu
import           Data.Conduit
import           Data.Conduit.List                       (consume)
-- import           Data.Monoid                            (mconcat, mappend)

import           SecondTransfer.MainLoop.CoherentWorker  (CoherentWorker,Headers)
import           SecondTransfer.MainLoop.PushPullType    (Attendant)
import           SecondTransfer.Sessions.Internal        (SessionsContext, acquireNewSessionTag)

-- Logging utilities
import           System.Log.Logger                      

import           SecondTransfer.Http1.Parse
import           SecondTransfer.Exception                (IOProblem)
import           SecondTransfer.Sessions.Config
import           SecondTransfer.Sessions.Internal        (sessionsConfig)
import qualified SecondTransfer.Utils.HTTPHeaders        as He

-- import           Debug.Trace                             (traceShow)


-- | Session attendant that speaks HTTP/1.1
-- 
http11Attendant :: SessionsContext -> CoherentWorker -> Attendant
http11Attendant sessions_context coherent_worker 
                push_action pull_action close_action 
    = do 
        new_session_tag <- acquireNewSessionTag sessions_context
        infoM "Session.Session_HTTP11" $ "Starting new session with tag: " ++(show new_session_tag)
        forkIO $ go new_session_tag (Just "")
        return ()
  where 
    go :: Int -> Maybe B.ByteString -> IO ()
    go session_tag (Just leftovers) = do 
        infoM "Session.Session_HTTP11" $ "(Re)Using session with tag: " ++(show session_tag)
        maybe_leftovers <- add_data newIncrementalHttp1Parser leftovers session_tag
        go session_tag maybe_leftovers

    go _ Nothing = 
        return ()

    add_data :: IncrementalHttp1Parser  -> B.ByteString -> Int -> IO (Maybe B.ByteString)
    add_data parser bytes session_tag = do 
        let 
            completion = addBytes parser bytes 
            -- completion = addBytes parser $ traceShow ("At session " ++ (show session_tag) ++ " Received: " ++ (unpack bytes) ) bytes
        case completion of 

            MustContinue_H1PC new_parser -> do 
                -- print "MustContinue_H1PC"
                catch 
                    (do
                        new_bytes <- pull_action
                        r <- add_data new_parser new_bytes session_tag
                        return r
                    )
                    ( (\ _e -> do
                        -- This is a pretty harmless condition that happens 
                        -- often when the remote peer closes the connection
                        debugM "Session.HTTP1" "Could not receive data"
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
                (response_headers, _, data_and_conclusion) <- coherent_worker (
                        modified_headers, 
                        Nothing
                    )
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
                        debugM "Session.HTTP1" "Session abandoned"
                        close_action
                        return Nothing
                    ) :: IOProblem -> IO (Maybe B.ByteString) )

            HeadersAndBody_H1PC _headers _stopcondition _recv_leftovers -> do
                -- print "HeadersAndBody_H1PC"
                -- Let's see if I can go through the basic movements first, then through 
                -- more complicated things.
                -- TODO: Implement posts and other requests with bodies....
                close_action
                error "NotImplemented requests with bodies"


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