{-# LANGUAGE OverloadedStrings, TemplateHaskell, FunctionalDependencies, FlexibleContexts, Rank2Types #-}

module SecondTransfer.FastCGI.GenericSession (
                 ioProxyToConnection
               , SessionConfig                                            (..)
               , documentRoot_cgiSC

               , SessionSeed                                              (..)
               , requestId_GeS
               , ioc_GeS
               , config_GeS

               , framesSource
               , framesParser
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

import           Data.Char                                                 (toLower, toUpper, isLower)
import           Data.Conduit
import qualified Data.Conduit.List                                         as CL
import qualified Data.Conduit.Attoparsec                                   as DCA
import           System.FilePath                                           ( (</>) )

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
                                                                           --HeaderValue,
                                                                           AwareWorkerStack,
                                                                           Header)
import           SecondTransfer.FastCGI.Records


type RawFilePath = B.ByteString


data SessionConfig = SessionConfig {
    _documentRoot_cgiSC     :: Maybe RawFilePath
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
        push = \ d -> do
            -- putStrLn $ "push " ++ show (LB.length d)
            session_seed ^. ( ioc_GeS . pushAction_IOC) $ d

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
        (do
            yield encoded_params
            yield ""
        )
        $$
        (toWrappedStream Params_RT request_id)
        =$=
        (CL.mapM_  (liftIO `fmap` push ) )
        )


framesParser :: (MonadCatch m) => Conduit B.ByteString m RecordFrame
framesParser =
    DCA.conduitParser readRecordFrame
    =$=
    CL.map snd


-- | It yields the payload of frames with a type Stdout_RT.
--   ATTENTION(IMPORTANT): No control for request_id is being made
--   here.
framesSource ::
    (MonadIO m, MonadCatch m) =>
    IO B.ByteString ->
    Source  m B.ByteString
framesSource bepa =
  let
        conn_source = do
            -- Will throw exceptions
            b <- liftIO bepa
            yield b
            conn_source

        frames_interpreter =
          do
            maybe_frame <- await
            case maybe_frame of
                Just frame -> case frame ^. type_RH of
                    Stderr_RT -> do
                        -- TODO: Properly propagate to the server logs.
                        -- See #26
                        -- liftIO $
                        --     putStrLn $
                        --         "ErrorData FastCGI: " ++ show (frame ^. payload_RH)
                        frames_interpreter
                    Stdout_RT -> do
                        let
                            payload = frame ^. payload_RH
                        if B.length payload > 0
                          then do
                            yield payload
                            frames_interpreter
                          else do
                            return ()
                    EndRequest_RT -> do
                        return ()
                    _fr -> do
                        -- Discard frames I can't undestand.
                        frames_interpreter

                Nothing -> do
                    return ()

  in
    (
        conn_source
        =$=
        framesParser
        =$=
        frames_interpreter
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

    -- Send the input
    if methodHasRequestBody method
      then do
        _ <- resourceForkIOExc "fastCGIOutputThread" $
            (
                client_input
                =$=
                CL.map LB.fromStrict
                $$
                (toWrappedStream Stdin_RT request_id)
                =$=
                (CL.mapM_  (liftIO `fmap` push ) )
                )
        return ()
      else do
        -- return ()
        (
            (yield "")
            $$
            (toWrappedStream Stdin_RT request_id)
            =$=
            (CL.mapM_  (liftIO `fmap` push ) )
            )

    -- Receive the headers as soon as possible, and prepare to receive
    -- any data to the application and forward it immediately to the inner side.
    (resumable_source, (headers, do_with_body)) <-
        framesSource  bepa
        $$+
        processHttp11OutputFromPipe method

    (source, finalizer) <-  unwrapResumable resumable_source

    let

        unwrapped_plain :: Source AwareWorkerStack B.ByteString
        unwrapped_plain = do
            -- liftIO $ putStrLn "entering plain"
            (
                source =$=
                CL.filter (\x -> B.length x > 0) =$=
                CL.mapM ( \x -> do
                       -- liftIO $ putStrLn $ " fetched length = " ++ show (B.length x)
                       return x
                    )
                )
            -- liftIO $ putStrLn "exiting plain"
            lift finalizer

        unwrapped_chunked = do
            -- liftIO $ putStrLn "entering chunked"
            source =$= CL.filter (\x -> B.length x > 0) =$= unwrapChunks
            -- liftIO $ putStrLn "exiting chunks"
            lift finalizer

    return $ case do_with_body of
        None_RBH   -> HttpResponse {
            _headers_Rp = headers
          , _body_Rp = do
                            return ()
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


capitalize :: Header -> Header
capitalize o@(hn, hv) =
  let
    maybe_low =  Ch8.unpack $ hn
    high =  map toUpper maybe_low
    high2 ='H':'T':'T':'P':'_': replaceUnderscore high
    replaceUnderscore ('-': rest) = '_' : replaceUnderscore rest
    replaceUnderscore (ch: rest) = ch : replaceUnderscore rest
    replaceUnderscore [] = []
    has_lower = any isLower maybe_low
  in if has_lower then (Ch8.pack high2, hv) else o


-- | We really need some value for the document root so that we can reason with
--   things like PHP.
requestHeadersToCGI :: Maybe B.ByteString -> He.Headers -> He.Headers
requestHeadersToCGI maybe_document_root headers_http =
  let
    t1 :: Header -> Header
    t1 =  over _1 (Ch8.pack . map toLower . Ch8.unpack)
    h0 = map t1 headers_http
    --
    basic_transformer_0 :: Header -> Header
    basic_transformer_0 =
        shortCircuit ":authority" "HTTP_HOST" .
        shortCircuit ":method" "REQUEST_METHOD" .
        shortCircuit ":scheme" "SCHEME" .
        shortCircuit ":path" "HTTP_URI_PATH" .
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
    h10 = map basic_transformer_0 h0

    h100 = filter (
        notThis ":path"         .
        notThis ":authority"    .
        notThis "host"          .
        notThis "path_info"     .   -- for security
        notThis "path"          .
        notThis "document_root" .
        notThis "script_name"   $
        const True
        ) h10

    h2 = h100 `seq` map capitalize h10

    -- We need a path
    path = case lookup ":path" headers_http of
        Nothing -> "/"  -- Actually invalid, but this is too late to be raising
                        -- exceptions
        Just p -> p

    (fcgi_uri, fcgi_query) = Ch8.span (/= '?') path
    no_start =
        let
           (fch, rest) = B.splitAt 1 fcgi_uri
        in
           if B.length fcgi_uri > 0
             then
               if fch == "/" then sanitize rest
                             else sanitize fcgi_uri
             else
               fcgi_uri

    Just document_root = maybe_document_root

    script_filename = Ch8.pack $  Ch8.unpack document_root </> Ch8.unpack  no_start
     -- Now let's drop some interesting bits, like the :path


    h3 = case maybe_document_root of
        Just _ -> ( if B.length fcgi_query > 0
                           then ( ("QUERY_STRING", B.drop 1 fcgi_query) : )
                           else id
                      ) $
                      ("REQUEST_URI",  path)           :
                      ("SCRIPT_NAME", fcgi_uri)        :
                      ("SCRIPT_FILENAME", script_filename) :
                      ("HTTPS", "on")                  :   -- TODO: Fix this
                      ("DOCUMENT_ROOT", document_root) : h2

        Nothing -> ( if B.length fcgi_query > 0
                           then ( ("QUERY_STRING", B.drop 1 fcgi_query) : )
                           else id
                      ) $
                      ("REQUEST_URI",  path)           :
                      ("SCRIPT_NAME", fcgi_uri)        :
                      ("HTTPS", "on")                  : h2

  in h3


sanitize :: B.ByteString -> B.ByteString
sanitize bs = Ch8.pack . go0 . Ch8.unpack $ bs
  where
    go0 :: String -> String
    go0 ('/':rest) = '/' : danger0 rest
    go0 (x: rest) = x : go0 rest
    go0 [] = []

    danger0 ('.': rest) = '.' : danger1 rest
    danger0 ('/': rest) = danger0 rest
    danger0 (x: rest ) = x: go0 rest
    danger0 [] = []

    -- Just break the spell
    danger1 ('/': rest) = 'I':'N':'V' : go0 rest
    danger1 ('.': rest) = 'I':'N':'V' : go0 rest
    danger1 (x : rest ) = x : go0 rest
    danger1 [] = []
