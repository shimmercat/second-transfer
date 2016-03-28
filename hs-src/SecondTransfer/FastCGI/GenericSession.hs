{-# LANGUAGE OverloadedStrings, TemplateHaskell, FunctionalDependencies, FlexibleContexts, Rank2Types #-}

module SecondTransfer.FastCGI.GenericSession (
                 ioProxyToConnection
               , SessionConfig                                            (..)
               , documentRoot_cgiSC

               , SessionSeed                                              (..)
               , requestId_GeS
               , ioc_GeS
               , config_GeS
    ) where

import           Control.Lens
import qualified Control.Exception                                         as E
import           Control.Monad                                             (when)
import           Control.Concurrent                                        hiding (yield)
import           Control.Monad.Morph                                       (hoist, lift)
import           Control.Monad.IO.Class                                    (liftIO, MonadIO)
import qualified Control.Monad.Trans.Resource                              as ReT
import           Control.Monad.Catch                                       (MonadCatch)

import qualified Data.ByteString                                           as B
import           Data.Maybe                                                (fromMaybe)
--import           Data.List                                                 (foldl')
import qualified Data.ByteString.Builder                                   as Bu
--import           Data.ByteString.Char8                                     (pack, unpack)
import qualified Data.ByteString.Char8                                     as Ch8
import qualified Data.ByteString.Lazy                                      as LB
import qualified Data.Binary                                               as Bin
import qualified Data.Binary.Put                                           as Bin

import           Data.Char                                                 (toLower)
import           Data.Conduit
import qualified Data.Conduit.List                                         as CL
import qualified Data.Conduit.Attoparsec                                   as DCA

import qualified Data.ByteString                                           as B

import           SecondTransfer.IOCallbacks.Types

-- The Http1 variation is as useful for this case, as we are not using the
-- neat features of HTTP/2
import           SecondTransfer.Http1.Types
import           SecondTransfer.Http1.Proxy                               (
                                                                           processHttp11OutputFromPipe
                                                                          )
import           SecondTransfer.Http1.Parse                               (
                                                                           methodHasRequestBody
                                                                          ,unwrapChunks
                                                                          )

import qualified SecondTransfer.Utils.HTTPHeaders                          as He
import           SecondTransfer.Exception                                  (resourceForkIOExc)
import           SecondTransfer.MainLoop.CoherentWorker                    (Headers,
                                                                           HeaderName,
                                                                           HeaderValue,
                                                                           AwareWorkerStack,
                                                                           Header)
import           SecondTransfer.FastCGI.Records


type RawFilePath = B.ByteString


data SessionConfig = SessionConfig {
    _documentRoot_cgiSC     :: RawFilePath
    }
    deriving Show

makeLenses ''SessionConfig


data SessionSeed = SessionSeed {
    _requestId_GeS          :: Int
  , _ioc_GeS                :: IOCallbacks
  , _config_GeS             :: SessionConfig
    }

makeLenses ''SessionSeed


ioProxyToConnection ::
   SessionSeed ->
   HttpRequest AwareWorkerStack ->
   AwareWorkerStack (HttpResponse AwareWorkerStack)
ioProxyToConnection session_seed  request =
  do
    let
        request_id = session_seed ^. requestId_GeS
        ioc = session_seed ^. ioc_GeS
        h3 = request ^. headers_Rq
        method = fromMaybe "GET" $ He.fetchHeader h3 ":method"

    -- For the time being, we are not going to set a PATH_INFO.
    sendHeadersToApplication session_seed h3

    --
    http_response <- processOutputAndStdErr
        request_id
        method
        (request ^. body_Rq )
        ioc

    return (http_response)


sendHeadersToApplication ::
  SessionSeed -> Headers -> AwareWorkerStack ()
sendHeadersToApplication session_seed http_headers =
  do
    let
        request_id = session_seed ^. requestId_GeS
        document_root = session_seed ^. config_GeS . documentRoot_cgiSC
        push = session_seed ^. ( ioc_GeS . pushAction_IOC)
        -- Packet with the begin request.... this is a Bin.Put
        begin_request_packet :: Bin.Put
        begin_request_packet =
            putBeginRequest $
                BeginRequest_Rec { _applicationClosesConnection_BR = False }
        begin_request_packet_lb =
            Bin.runPut begin_request_packet

        wrapped_record_frame_pu =
            wrapRecordFrame
                BeginRequest_RT
                request_id
                begin_request_packet_lb

        wrapped_record_frame_lb = Bin.runPut wrapped_record_frame_pu

    -- Open the request with that little frame
    liftIO $ push wrapped_record_frame_lb

    -- Now send a bunch of frames with the headers
    let
        params_as_such = requestHeadersToCGI document_root http_headers
        encoded_params =
          Bu.toLazyByteString . mconcat . map writeParameterPair $ params_as_such

    (
        (yield encoded_params)
        $$
        (toWrappedStream Params_RT request_id)
        =$=
        (CL.mapM_  (liftIO `fmap` push ) )
        )


processOutputAndStdErr ::
    Int ->
    B.ByteString ->
    Source AwareWorkerStack B.ByteString ->
    IOCallbacks ->
        AwareWorkerStack (HttpResponse AwareWorkerStack)
processOutputAndStdErr request_id method client_input ioc =
  do
    let
        push = ioc ^. pushAction_IOC
        bepa = (ioc ^. bestEffortPullAction_IOC) True

        conn_source = do
            -- Will throw exceptions
            b <- liftIO bepa
            yield b
            conn_source

        frames_parser =
            DCA.conduitParser readRecordFrame
            =$=
            CL.map snd

        frames_interpreter =
          do
            maybe_frame <- await
            case maybe_frame of
                Just frame -> case frame ^. type_RH of
                    EndRequest_RT -> return ()
                    Stdout_RT -> do
                        yield $ frame ^. payload_RH
                        frames_interpreter
                    _ ->
                        -- Discard frames I can't undestand.
                        frames_interpreter

    -- TODO: Check method has input etc.
    _ <- resourceForkIOExc "fastCGIOutputThread" $
        if methodHasRequestBody method
          then
            (
                client_input
                =$=
                CL.map LB.fromStrict
                $$
                (toWrappedStream Stdin_RT request_id)
                =$=
                (CL.mapM_  (liftIO `fmap` push ) )
                )
          else
            (
                (yield "")
                $$
                (toWrappedStream Stdin_RT request_id)
                =$=
                (CL.mapM_  (liftIO `fmap` push ) )
                )

    (resumable_source, (headers, do_with_body)) <-
        (
          conn_source
          =$=
          frames_parser
          =$=
          frames_interpreter
        )
        $$+
        processHttp11OutputFromPipe method

    let

        unwrapped_plain :: Source AwareWorkerStack B.ByteString
        unwrapped_plain = do
            (source, finalizer) <- lift $ unwrapResumable resumable_source
            source
            lift finalizer

        unwrapped_chunked = do
            (source, finalizer) <- lift $ unwrapResumable resumable_source
            source =$= unwrapChunks
            lift finalizer

    return $ case do_with_body of
        None_RBH   -> HttpResponse {
            _headers_Rp = headers
          , _body_Rp = return ()
            }

        PlainBytes_RBH -> HttpResponse {
            _headers_Rp = headers
          , _body_Rp = unwrapped_plain
            }

        Chunked_RBH -> HttpResponse {
            _headers_Rp = headers
          , _body_Rp = unwrapped_chunked
            }




shortCircuit :: HeaderName -> HeaderName   -> (Header -> Header) -> (Header -> Header )
shortCircuit from_name to_name f =
  let
    transform' h@(hn, hv) | hn == from_name  =  (to_name, hv)
                         | otherwise = f h
  in transform'


notThis :: HeaderName -> (Header -> Bool) -> (Header -> Bool)
notThis forbid_name f =
  let
    test h@(hn, _hv) | hn == forbid_name = False
                     | otherwise = f h
  in test


-- | We really need some value for the document root so that we can reason with
--   things like PHP.
requestHeadersToCGI :: B.ByteString -> He.Headers -> He.Headers
requestHeadersToCGI document_root headers_http =
  let
    t1 :: Header -> Header
    t1 =  over _1 (Ch8.pack . map toLower . Ch8.unpack)
    h0 = map t1 headers_http
    --
    basic_transformer_0 :: Header -> Header
    basic_transformer_0 =
        shortCircuit "cookie" "HTTP_COOKIE" .
        shortCircuit ":authority" "HTTP_HOST" .
        shortCircuit "host" "HTTP_HOST" .
        shortCircuit "referer" "HTTP_REFERER" .
        shortCircuit "user-agent" "HTTP_USER_AGENT" .
        shortCircuit ":method" "REQUEST_METHOD" .
        shortCircuit "content-length" "CONTENT_LENGTH" .
        shortCircuit "accept" "HTTP_ACCEPT" .
        shortCircuit "accept-encoding" "HTTP_ACCEPT_ENCODING" .
        shortCircuit "accept-language" "HTTP_ACCEPT_LANGUAGE" .
        -- From here on, we need some help to get these expressed.
        -- TODO:  Create a function that translates the perception to enhanced
        -- headers ....
        shortCircuit "sc-remote-addr"     "REMOTE_ADDR" .
        shortCircuit "sc-remote-host"     "REMOTE_HOST" .
        shortCircuit "sc-remote-protocol" "REMOTE_PROTOCOL" .
        shortCircuit "sc-push-enabled"    "REMOTE_PUSH_ENABLED" .
        shortCircuit "sc-server-name"     "SERVER_NAME" $
        id

    -- Let's go for the first iteration
    h1 = map basic_transformer_0 h0

    -- We need a path
    path = case lookup ":path" headers_http of
        Nothing -> "/"  -- Actually invalid, but this is too late to be raising
                        -- exceptions
        Just p -> p

    (fcgi_uri, fcgi_query) = Ch8.span (/= '?') path
     -- Now let's drop some interesting bits, like the :path
    h2 = filter (
        notThis ":path"         .
        notThis ":authority"    .
        notThis "host"          .
        notThis "path_info"     .   -- for security
        notThis "path"          .
        notThis "document_root" .
        notThis "script_name"   $
        const True
        ) h1

    h3 = ("QUERY_STRING", fcgi_query)     :
         ("REQUEST_URI",  fcgi_uri)       :
         ("HTTPS", "on")                  :   -- TODO: Fix this
         ("DOCUMENT_ROOT", document_root) : h2

  in h3
