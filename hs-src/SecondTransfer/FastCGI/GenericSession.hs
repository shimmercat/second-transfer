{-# LANGUAGE
     OverloadedStrings,
     TemplateHaskell,
     FunctionalDependencies,
     FlexibleContexts,
     Rank2Types,
     PartialTypeSignatures #-}
module SecondTransfer.FastCGI.GenericSession (
                 ioProxyToConnection
               , SessionConfig                                            (..)
               , documentRoot_cgiSC
               , errorReporting_cgiSC

               , SessionSeed                                              (..)
               , requestId_GeS
               , ioc_GeS
               , config_GeS

               , framesSource
               , framesParser
    ) where

import           Control.Lens
import qualified Control.Exception                                         as E
--import           Control.Monad                                             (when)
--import           Control.Concurrent                                        hiding (yield)
import           Control.Monad.Morph                                       (
                                                                            --hoist,
                                                                            lift
                                                                           )
import           Control.Monad.Trans.Control                               (MonadBaseControl)
import           Control.Monad.IO.Class                                    (liftIO, MonadIO)
--import qualified Control.Monad.Trans.Resource                              as ReT
import           Control.Monad.Catch                                       (throwM, MonadThrow, MonadCatch)
import qualified Control.Monad.Catch                                       as CMC

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
import qualified Data.Attoparsec.ByteString                                as ATO
import           System.FilePath                                           ( (</>) )

import           SimpleHttpHeadersHq

import           SecondTransfer.IOCallbacks.Types

-- The Http1 variation is as useful for this case, as we are not using the
-- neat features of HTTP/2
import           SecondTransfer.Http1.Types
import           SecondTransfer.Utils.HTTPHeaders
import           SecondTransfer.Http1.Proxy                               (
                                                                           processHttp11OutputFromPipe
                                                                          )
import           SecondTransfer.Http1.Parse                               (
                                                                           methodHasRequestBody'
                                                                          ,unwrapChunks
                                                                          )

--import qualified SecondTransfer.Utils.HTTPHeaders                          as He
import           SecondTransfer.Exception
import           SecondTransfer.MainLoop.CoherentWorker                   (
                                                                           AwareWorkerStack
                                                                           )
import           SecondTransfer.FastCGI.Records


type RawFilePath = B.ByteString


data SessionConfig = SessionConfig {
    _documentRoot_cgiSC     :: Maybe RawFilePath
  , _errorReporting_cgiSC   :: ! (B.ByteString -> IO ())
    }


makeLenses ''SessionConfig

instance Show SessionConfig where
   show sc = "SessionConfig { _documentRoot= " ++
              show (sc ^. documentRoot_cgiSC) ++ "}"


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
        method = fromMaybe Get_HtM $  h3 ^. method_Hi
        error_clbk = \ err_str ->
            liftIO $ (session_seed ^. config_GeS . errorReporting_cgiSC) err_str

    sendHeadersToApplication session_seed h3

    http_response <-  processOutputAndStdErr
       request_id
       method
       (request ^. body_Rq )
       ioc
       error_clbk

    return http_response


sendHeadersToApplication ::
  SessionSeed -> HqHeaders -> AwareWorkerStack ()
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


-- | It yields the payload of frames with a type Stdout_RT, and logs
--   the payload of error frames.
--   ATTENTION(IMPORTANT): No control for request_id is being made
--   here.
framesSource ::
    (MonadIO m, MonadCatch m) =>
    IO LB.ByteString ->
    (B.ByteString -> m () ) ->
    Source  m B.ByteString
framesSource
    bepa
    error_clbk
  =
  let
        conn_source = do
            -- Will throw exceptions
            b <- liftIO bepa
            CL.sourceList $ LB.toChunks b
            conn_source

        frames_interpreter :: Conduit RecordFrame _ B.ByteString
        frames_interpreter =
          do
            maybe_frame <- await
            case maybe_frame of
                Just frame -> case frame ^. type_RH of
                    Stderr_RT -> do
                        lift $ error_clbk (frame ^. payload_RH)
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
                        -- ATTENTION: THE CODE BELOW IS GOOD!, it is just
                        --  that we need to create an EXCEPTION TYPE HERE
                        -- to raise when there are errors in the backend.
                        let
                            payload = frame ^. payload_RH
                            either_parsed_end = ATO.parseOnly
                               readEndRequest
                               payload
                        case either_parsed_end of
                            Left msg ->
                                 lift $ error_clbk "FastCGI end-request packet was not correctly parsed"
                            _ ->
                                 return ()
                    _fr -> do
                        -- Discard frames I can't undestand.
                        liftIO $ putStrLn  "FASTCGI frame doesn't make sense"
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
    HttpMethod ->
    Source AwareWorkerStack B.ByteString ->
    IOCallbacks ->
    (B.ByteString -> AwareWorkerStack ()) ->
        AwareWorkerStack (HttpResponse AwareWorkerStack)
processOutputAndStdErr
    request_id
    method
    client_input
    ioc
    error_clbk
  =
  do
    let
        push = ioc ^. pushAction_IOC
        bepa = (ioc ^. bestEffortPullAction_IOC) True
        method_str = LB.toStrict . Bu.toLazyByteString . toHeaderValue $ method

    -- Send the input
    if methodHasRequestBody' method
      then do
        _ <- resourceForkIOExc "fastCGIOutputThread" $ do
            CMC.catchAll
                (
                    client_input
                    =$=
                    CL.map LB.fromStrict
                    $$
                    (toWrappedStream Stdin_RT request_id) -- Takes care of end-of-stream
                    =$=
                    (CL.mapM_  (liftIO `fmap` push ) )
                    )
                -- TO-DO: Find a better way of propagating this exception
                (\ exc -> liftIO . putStrLn $ "EXCEPTION trasvasing input to backend: " ++ show exc)

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
        framesSource  bepa error_clbk
        $$+
        processHttp11OutputFromPipe method_str

    (source, finalizer) <-  unwrapResumable resumable_source

    let

        unwrapped_plain :: Source AwareWorkerStack B.ByteString
        unwrapped_plain = do
            (
                source =$=
                CL.filter (\x -> B.length x > 0) =$=
                CL.mapM ( \x -> do
                       -- liftIO $ putStrLn $ " fetched length = " ++ show (B.length x)
                       return x
                    )
                )

            lift finalizer

        unwrapped_chunked  = do
            source =$=
                   CL.filter (\x -> B.length x > 0)
                   =$=
                   (catchC
                      unwrapChunks
                      ((\ e ->  do
                            throwM . ForwardedGatewayException . E.toException $ e
                       ) :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) => HTTP11SyntaxException -> ConduitM B.ByteString B.ByteString m () )
                  )
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


type Header' = (B.ByteString, B.ByteString)

shortCircuit :: B.ByteString -> B.ByteString   -> (Header' -> Header') -> (Header' -> Header' )
shortCircuit from_name to_name f =
  let
    transform' h@(hn, hv) | hn == from_name  =  (to_name, hv)
                          | otherwise = f h
  in transform'


notThis :: B.ByteString -> (Header' -> Bool) -> (Header' -> Bool)
notThis forbid_name f =
  let
    test h@(hn, _hv) | hn == forbid_name = False
                     | otherwise = f h
  in test


capitalize :: Header' -> Header'
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
requestHeadersToCGI :: Maybe B.ByteString -> HqHeaders -> [Header']
requestHeadersToCGI maybe_document_root headers =
  let
    headers_http = headers' ^. serialized_HqH
    headers' = promoteHost headers
    t1 :: Header' -> Header'
    t1 =  over _1 (Ch8.pack . map toLower . Ch8.unpack)
    h0 = map t1 headers_http
    --
    basic_transformer_0 :: Header' -> Header'
    basic_transformer_0 =
        shortCircuit ":authority" "HTTP_HOST" .
        shortCircuit ":method" "REQUEST_METHOD" .
        shortCircuit ":scheme" "SCHEME" .
        shortCircuit ":path" "HTTP_URI_PATH" .
        shortCircuit "content-length"     "CONTENT_LENGTH" .
        shortCircuit "content-type"       "CONTENT_TYPE" .
        -- From here on, we need some help to get these expressed.
        -- TODO:  Create a function that translates the perception to enhanced
        -- headers ....(Meaning that these headers are still not there)
        shortCircuit "sc-remote-addr"     "REMOTE_ADDR" .
        shortCircuit "sc-remote-host"     "REMOTE_HOST" .
        shortCircuit "sc-remote-protocol" "SERVER_PROTOCOL" .
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
        notThis "request-uri"   .
        notThis "document_root" .
        notThis "proxy"         .   -- HTTPOXY security vulnerability
        notThis "script_name"   $
        const True
        ) h10

    h2 =  map capitalize h100

    -- We need a path
    path =
        case lookup ":path" headers_http of
            Nothing -> "/"  -- Actually invalid, but this is too late to be raising
                            -- exceptions
            Just p -> p

    -- And a request uri
    request_uri =
        case lookup "request-uri" headers_http of
            Nothing -> path
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

    -- Below: only used conditionally
    Just document_root = maybe_document_root
    script_filename = Ch8.pack $  Ch8.unpack document_root </> Ch8.unpack  no_start
     -- Now let's drop some interesting bits, like the :path


    h3 = case maybe_document_root of
        Just _ -> ( if B.length fcgi_query > 0
                           then ( ("QUERY_STRING", B.drop 1 fcgi_query) : )
                           else id
                      ) $
                      ("REQUEST_URI",  request_uri)    :
                      ("SCRIPT_NAME", fcgi_uri)        :
                      ("SERVER_PROTOCOL", "HTTP/1.1")  :   -- A big fat lie, but god knows why Wordpress is querying this variable
                      ("SCRIPT_FILENAME", script_filename) :
                      ("HTTPS", "on")                  :   -- TODO: Fix this
                      ("PATH_INFO", "")                :
                      ("DOCUMENT_ROOT", document_root) : h2

        Nothing -> ( if B.length fcgi_query > 0
                           then ( ("QUERY_STRING", B.drop 1 fcgi_query) : )
                           else id
                      ) $
                      ("REQUEST_URI",  request_uri)    :
                      ("SERVER_PROTOCOL", "HTTP/1.1")  :
                      ("SCRIPT_NAME", fcgi_uri)        :
                      ("PATH_INFO", "")                :
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
