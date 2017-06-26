{-# LANGUAGE OverloadedStrings, TemplateHaskell, FunctionalDependencies, FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
module SecondTransfer.Http1.Proxy (
                 ioProxyToConnection
               , processHttp11Output

               , processHttp11OutputFromPipe
               , ResponseBodyHandling                                      (..)
        ) where

import           Control.Lens
import qualified Control.Exception                                         as E
import           Control.Monad                                             (when)
--import           Control.Monad.Morph                                       (hoist, lift)
import           Control.Monad.IO.Class                                    (liftIO, MonadIO)
import           Control.Monad.Trans.Control                               (MonadBaseControl)
import           Control.Monad.Catch                                       (throwM, MonadThrow)
--import qualified Control.Monad.Trans.Resource                              as ReT

import qualified Data.ByteString                                           as B
--import           Data.List                                                 (foldl')
import qualified Data.ByteString.Builder                                   as Bu
--import           Data.ByteString.Char8                                     (pack, unpack)
--import qualified Data.ByteString.Char8                                     as Ch8
import qualified Data.ByteString.Lazy                                      as LB
--import           Data.Char                                                 (toLower)
import           Data.Maybe                                                (fromMaybe)

import           Data.Conduit
import           Data.Conduit.List                                         as CL

import           SimpleHttpHeadersHq

import qualified SecondTransfer.Utils.HTTPHeaders                          as He

import           SecondTransfer.Http1.Types
import           SecondTransfer.Http1.Parse                                (
                                                                              headerListToHTTP1RequestText
                                                                            , methodHasRequestBody
                                                                            , methodHasResponseBody
                                                                            , newIncrementalHttp1Parser
                                                                            --, IncrementalHttp1Parser
                                                                            , Http1ParserCompletion(..)
                                                                            , addBytes
                                                                            , unwrapChunks
                                                                            , leftoversFromParserCompletion
                                                                            , BodyStopCondition(..)
                                                                            )
import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.IOCallbacks.Coupling                       (sendSourceToIO)
import           SecondTransfer.Exception                                  (
                                                                              HTTP11SyntaxException(..)
                                                                            , NoMoreDataException
                                                                            , ForwardedGatewayException(..)
                                                                            , IOProblem (..)
                                                                            , GatewayAbortedException (..)
                                                                            , keyedReportExceptions
                                                                            -- , ignoreException
                                                                            -- , ioProblem
                                                                           )

#include "instruments.cpphs"


-- | Takes an IOCallbacks  and serializes a request (encoded HTTP/2 style in headers and streams)
--   on top of the callback, waits for the results, and returns the response. Notice that this proxy
--   may fail for any reason, do take measures and handle exceptions.
--
--  The use of a generic monad for the operations helps by making ResourceT a possibility.
--
--   Also, most headers manipulations
--   (e.g. removing the Connection header) are left to the upper layers. And this doesn't include
--   managing any kind of pipelining in the http/1.1 connection, however, close is not done, so
--   keep-alive (not pipelineing) should be OK.
ioProxyToConnection :: forall m . (MonadIO m, MonadBaseControl IO m, MonadThrow m) => IOCallbacks -> HttpRequest m -> m (HttpResponse m)
ioProxyToConnection ioc request =
  do
    let
       h1 = request ^. headers_Rq
       h2 = He.combineAuthorityAndHost h1
       headers_bu = headerListToHTTP1RequestText h2
       separator = "\r\n"

       -- Contents of the head, including the separator, which should always
       -- be there.
       cnt1 = headers_bu `mappend` separator
       cnt1_lbz = Bu.toLazyByteString cnt1

       -- The string representation of it
       method =
           LB.toStrict .
           Bu.toLazyByteString .
           toHeaderValue .
           fromMaybe Get_HtM $  h2 ^. method_Hi

    -- Send the headers and the separator

    -- This code can throw an exception, in that case, just let it
    -- bubble. But the upper layer should deal with it.
    liftIO $ (ioc ^. pushAction_IOC) cnt1_lbz

    -- Send the rest only if the method has something ....
    if methodHasRequestBody method
      then
        -- We also need to send the body
        sendSourceToIO  (mapOutput LB.fromStrict $ request ^. body_Rq)  ioc
      else
        return ()

    processHttp11Output
        ( (ioc ^. bestEffortPullAction_IOC ) True)
        method


-- | Takes something that produces ByteStrings and reads from there an HTTP/1.1
--   response. Notice
--   that the returned structure handles the response body as a stream, therefore
--   this function creates some threads.
--   Use the fact that `m` can be any monad to control for resource deallocation
--   upon exceptions.
processHttp11Output ::
  (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
  IO LB.ByteString ->
  B.ByteString ->
  m (HttpResponse m)
processHttp11Output bepa method =
  do
    -- So, say that we are here, that means we haven't exploded
    -- in the process of sending this request. now let's Try to
    -- fetch the answer...
    let
        incremental_http_parser = newIncrementalHttp1Parser

        -- pump0 :: IncrementalHttp1Parser -> m Http1ParserCompletion
        pump0 p =
         do
            some_bytes <- liftIO bepa
            let completion = addBytes p some_bytes
            case completion of
               MustContinue_H1PC new_parser -> pump0 new_parser

               -- In any other case, just return
               a -> return a

        pumpout :: MonadIO m => B.ByteString -> Int -> Source m B.ByteString
        pumpout fragment n = do
            when (B.length fragment > 0) $  yield fragment
            when (n > 0 ) $ pull n

        -- Pull up to n bytes, but not one more. We could
        -- keep any un-consummed data in the pipe. That's pipelining for
        -- HTTP/1.1, which we are not using.  Here instead we trust
        -- that the underlying layer is somehow delimiting the output,
        -- so that (ioc ^ bestEffortPullAction_IOC ) True will return
        -- a packet that ends at the boundaries of HTTP/1.1 requests.
        -- Without HTTP/1.1 pipelining, that *must* happen, even over
        -- keepalive connections.
        pull :: MonadIO m => Int -> Source m B.ByteString
        pull n =
          do

            either_ioproblem_or_s <-
              liftIO $
                  keyedReportExceptions "pll-" $
                      E.try bepa

            case either_ioproblem_or_s :: Either IOProblem LB.ByteString of
                Left _exc -> liftIO $ E.throwIO GatewayAbortedException
                Right datum
                  | nn <- fromIntegral (LB.length datum), nn < n ->
                    do
                      CL.sourceList . LB.toChunks $ datum
                      pull (n - nn)
                  | otherwise ->
                      -- This will discard the rest of the data, read
                      -- comment above.
                      CL.sourceList .
                         LB.toChunks .
                         LB.take (fromIntegral n) $
                         datum

        pull_forever :: MonadIO m => Source m B.ByteString
        pull_forever = do
            either_ioproblem_or_s <-
              liftIO $
                  keyedReportExceptions "plc-" $
                      E.try bepa

            s <- case either_ioproblem_or_s :: Either IOProblem LB.ByteString of
                Left _exc -> liftIO $ E.throwIO GatewayAbortedException
                Right datum -> return datum
            CL.sourceList . LB.toChunks $ s

        unwrapping_chunked :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) => B.ByteString -> Source m B.ByteString
        unwrapping_chunked leftovers =
            (do
                yield leftovers
                pump_until_exception ""
            ) =$=
            (catchC
                unwrapChunks
                ((\ e ->  do
                      throwM . ForwardedGatewayException . E.toException $ e
                 ) :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) => HTTP11SyntaxException -> ConduitM B.ByteString B.ByteString m () )
            )

        pump_until_exception fragment = do

            if B.length fragment > 0
              then do
                yield fragment
                pump_until_exception mempty
              else do
                s <- liftIO $
                    keyedReportExceptions "ue-" $
                        E.try bepa

                case (s :: Either NoMoreDataException LB.ByteString) of
                    Left _ -> do
                        return ()

                    Right datum -> do
                        CL.sourceList . LB.toChunks $  datum
                        pump_until_exception mempty

    parser_completion <- pump0 incremental_http_parser

    case parser_completion of

        OnlyHeaders_H1PC headers leftovers -> do
            when (B.length leftovers > 0) $ do
                return ()
            return HttpResponse {
                _headers_Rp = headers
              , _body_Rp = return ()
                }

        HeadersAndBody_H1PC headers (UseBodyLength_BSC n) leftovers -> do
            --  HEADs must be handled differently!
            if methodHasResponseBody method
              then
                return HttpResponse {
                    _headers_Rp = headers
                  , _body_Rp = pumpout leftovers (n - (fromIntegral $ B.length leftovers ) )
                    }
              else
                return HttpResponse {
                    _headers_Rp = headers
                  , _body_Rp = return ()
                    }


        HeadersAndBody_H1PC headers Chunked_BSC  leftovers -> do
            --  HEADs must be handled differently!
            if methodHasResponseBody method
              then
                return HttpResponse {
                    _headers_Rp = headers
                  , _body_Rp = unwrapping_chunked leftovers
                    }
              else
                return HttpResponse {
                    _headers_Rp = headers
                  , _body_Rp = return ()
                    }


        HeadersAndBody_H1PC _headers SemanticAbort_BSC  _leftovers -> do
            --  HEADs must be handled differently!
            liftIO . reportHTTP1Error $ "SemanticAbort:SomethingAboutHTTP/1.1WasNotRight"


        HeadersAndBody_H1PC headers ConnectionClosedByPeer_BSC leftovers -> do
            -- The parser will assume that most responses have a body in the absence of
            -- content-length, and that's probably as well. We work around that for
            -- "HEAD" kind responses
            if methodHasResponseBody method
              then
                return HttpResponse {
                    _headers_Rp = headers
                  , _body_Rp = pump_until_exception leftovers
                    }
              else
                return HttpResponse {
                    _headers_Rp = headers
                  , _body_Rp = return ()
                    }

        MustContinue_H1PC _ ->
            error "UnexpectedIncompleteParse"

        -- TODO: See what happens when this exception passes from place to place.
        RequestIsMalformed_H1PC msg -> do
            liftIO . reportHTTP1Error $ msg


-- | Takes something that produces ByteStrings and reads from there an HTTP/1.1
--   response, and a something to do with the body
--   that the returned structure handles the response body as a stream, therefore
--   this function creates some threads.
--   Use the fact that `m` can be any monad to control for resource deallocation
--   upon exceptions.
processHttp11OutputFromPipe ::
  (MonadIO m, MonadThrow m) =>
  B.ByteString ->
  Sink B.ByteString m (HqHeaders, ResponseBodyHandling)
processHttp11OutputFromPipe  method =
  do
    -- So, say that we are here, that means we haven't exploded
    -- in the process of sending this request. now let's Try to
    -- fetch the answer...
    let
        incremental_http_parser = newIncrementalHttp1Parser

        -- pump0 :: IncrementalHttp1Parser -> m Http1ParserCompletion
        -- pump0  :: IncrementalHttp1Parser -> Sink B.ByteString m Http1ParserCompletion
        pump0 p =
         do
            maybe_some_bytes <- await
            case maybe_some_bytes of
                Nothing -> return $ RequestIsMalformed_H1PC "InputIsTooShort"
                Just some_bytes -> do
                    let completion = addBytes p $ LB.fromStrict some_bytes
                    case completion of
                       MustContinue_H1PC new_parser -> pump0 new_parser

                       -- In any other case, just return
                       a -> do
                           let
                               leftovers = leftoversFromParserCompletion a
                           leftover leftovers
                           return a

        -- unwrapChunks


    parser_completion <- pump0 incremental_http_parser

    case parser_completion of

        OnlyHeaders_H1PC headers _leftovers -> do
            return (headers, None_RBH)

        HeadersAndBody_H1PC headers (UseBodyLength_BSC _n) _leftovers -> do
            --  HEADs must be handled differently!
            if methodHasResponseBody method
              then
                return (headers, PlainBytes_RBH)

              else
                return (headers, None_RBH)

        HeadersAndBody_H1PC headers Chunked_BSC  _leftovers -> do
            --  HEADs must be handled differently!
            if methodHasResponseBody method
              then
                return (headers, Chunked_RBH)

              else
                return (headers, None_RBH)

        HeadersAndBody_H1PC _headers SemanticAbort_BSC  _leftovers -> do
            --  TODO: Check how this works in practice
            liftIO . reportHTTP1Error $ "SemanticAbort:SomethingAboutHTTP/1.1WasNotRight"

        HeadersAndBody_H1PC headers ConnectionClosedByPeer_BSC _leftovers -> do
            -- The parser will assume that most responses have a body in the absence of
            -- content-length, and that's probably as well. We work around that for
            -- "HEAD" kind responses
            if methodHasResponseBody method
              then
                return (headers, PlainBytes_RBH)
              else
                return (headers, None_RBH)

        MustContinue_H1PC _ ->
            error "UnexpectedIncompleteParse"

        -- TODO: See what happens when this exception passes from place to place.
        RequestIsMalformed_H1PC msg -> do
            liftIO . reportHTTP1Error $ msg


reportHTTP1Error ::  String -> IO a
reportHTTP1Error = E.throwIO . ForwardedGatewayException . E.toException . HTTP11SyntaxException
