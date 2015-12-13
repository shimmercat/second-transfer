{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, DeriveDataTypeable  #-}
-- | A CoherentWorker is one that doesn't need to compute everything at once...
--   This one is simpler than the SPDY one, because it enforces certain order....



module SecondTransfer.MainLoop.CoherentWorker(
    getHeaderFromFlatList
    , nullFooter

    , HeaderName
    , HeaderValue
    , Header
    , Headers
    , FinalizationHeaders
    , Request(..)
    , Footers
    , Perception(..)
    , Effect(..)
    , AwareWorker
    , PrincipalStream(..)
    , PushedStreams
    , PushedStream(..)
    , DataAndConclusion
    , CoherentWorker
    , InputDataStream
    , TupledPrincipalStream
    , TupledRequest
    , FragmentDeliveryCallback
    , InterruptEffect(..)

    , headers_RQ
    , inputData_RQ
    , perception_RQ
    , headers_PS
    , pushedStreams_PS
    , dataAndConclusion_PS
    , dataAndConclusion_Psh
    , requestHeaders_Psh
    , responseHeaders_Psh
    , effect_PS
    , startedTime_Pr
    , streamId_Pr
    , sessionId_Pr
    , anouncedProtocols_Pr
    , protocol_Pr
    , fragmentDeliveryCallback_Ef
    , priorityEffect_Ef
    , interrupt_Ef

    , defaultEffects
    , coherentToAwareWorker

    , tupledPrincipalStreamToPrincipalStream
    , requestToTupledRequest
    ) where


import           Control.Lens
import qualified Data.ByteString                       as B
import           Data.Conduit
import           Data.Foldable                         (find)
import           System.Clock                          (TimeSpec)

import           SecondTransfer.MainLoop.Protocol      (HttpProtocolVersion)


-- | The name part of a header
type HeaderName = B.ByteString

-- | The value part of a header
type HeaderValue = B.ByteString

-- | The complete header
type Header = (HeaderName, HeaderValue)

-- |List of headers. The first part of each tuple is the header name
-- (be sure to conform to the HTTP/2 convention of using lowercase)
-- and the second part is the headers contents. This list needs to include
-- the special :method, :scheme, :authority and :path pseudo-headers for
-- requests; and :status (with a plain numeric value represented in ascii digits)
-- for responses.
type Headers = [Header]

-- |This is a Source conduit (see Haskell Data.Conduit library from Michael Snoyman)
-- that you can use to retrieve the data sent by the peer piece-wise.
type InputDataStream = Source IO B.ByteString


-- | Data related to the request
data Perception = Perception {
    -- | The HTTP/2 stream id. Or the serial number of the request in an
    -- HTTP/1.1 session.
    _streamId_Pr :: Int,
    -- | A number uniquely identifying the session. This number is unique and
    --   the same for each TPC connection that a client opens using a given protocol.
    _sessionId_Pr :: Int,
    -- | Monotonic time close to when the request was first seen in
    -- the processing pipeline.
    _startedTime_Pr       :: TimeSpec,
    -- | Which protocol is serving the request
    _protocol_Pr          :: HttpProtocolVersion,
    -- | For new connections, probably a list of anounced protocols
    _anouncedProtocols_Pr :: Maybe [B.ByteString]
  }

makeLenses ''Perception


-- | A request is a set of headers and a request body....
-- which will normally be empty, except for POST and PUT requests. But
-- this library enforces none of that.
data Request = Request {
     _headers_RQ    :: ! Headers,
     _inputData_RQ  :: Maybe InputDataStream,
     _perception_RQ :: ! Perception
  }

makeLenses ''Request

-- | Finalization headers. If you don't know what they are, chances are
--   that you don't need to worry about them for now. The support in this
--   library for those are at best sketchy.
type FinalizationHeaders = Headers

-- | Finalization headers
type Footers = FinalizationHeaders

-- | A list of pushed streams.
--   Notice that a list of IO computations is required here. These computations
--   only happen when and if the streams are pushed to the client.
--   The lazy nature of Haskell helps to avoid unneeded computations if the
--   streams are not going to be sent to the client.
type PushedStreams = [ IO PushedStream ]

-- | A source-like conduit with the data returned in the response. The
--   return value of the conduit is a list of footers. For now that list can
--   be anything (even bottom), I'm not handling it just yet.
type DataAndConclusion = ConduitM () B.ByteString IO Footers


-- | A pushed stream, represented by a list of request headers,
--   a list of response headers, and the usual response body  (which
--   may include final footers (not implemented yet)).
data PushedStream = PushedStream {
  _requestHeaders_Psh    :: Headers,
  _responseHeaders_Psh   :: Headers,
  _dataAndConclusion_Psh :: DataAndConclusion
  }

makeLenses ''PushedStream


-- | First argument is the ordinal of this data frame, second an approximation of when
--   the frame was delivered, according to the monotonic clock. Do not linger in this call,
--   it may delay some important thread
type FragmentDeliveryCallback = Int -> TimeSpec -> IO ()


-- | Types of interrupt effects that can be signaled by aware workers. These include whole
--   connection shutdowns and stream resets. In all the cases, the reason given will be
--   NO_ERROR.
data InterruptEffect = InterruptConnectionAfter_IEf   -- ^ Close and send GoAway /after/ this stream finishes delivery
                       |InterruptConnectionNow_IEf    -- ^ Close and send GoAway /without/ delivering this stream.  This implies that
                                                      --   other fields of the PrincipalStream record will be ignored.
                       |CloseOnHttp1_IEf              -- ^ Close the connection if it is an HTTP/1.1 connection. This is for achieving
                                                      --   a cheap form of streaming without actually used chunked transfer encoding.
                    -- |InterruptThisStream_IEf       -- ^ Just reset this stream, disabled for now.


-- | Sometimes a response needs to be handled a bit specially,
--   for example by reporting delivery details back to the worker
data Effect = Effect {
  -- | A callback to be called whenever a data-packet for this stream is called.
  _fragmentDeliveryCallback_Ef :: Maybe FragmentDeliveryCallback

  -- | In certain circunstances a stream can use an internal priority,
  -- not given by the browser and the protocol. Lowest values here are
  -- given more priority. Default (when Nothing) is given zero. Cases
  -- with negative numbers also work.
  ,_priorityEffect_Ef :: Maybe Int

  -- | There are situations when it is desirable to close a stream or the entire
  --   connection. Use this member to indicate that.
  ,_interrupt_Ef :: Maybe InterruptEffect
  }

-- Quick instance of show
instance Show Effect where
    show _ = "<some-effect>"

makeLenses ''Effect

defaultEffects :: Effect
defaultEffects = Effect {
  _fragmentDeliveryCallback_Ef = Nothing,
  _priorityEffect_Ef = Nothing,
  _interrupt_Ef = Nothing
   }


-- | You use this type to answer a request. The `Headers` are thus response
--   headers and they should contain the :status pseudo-header. The `PushedStreams`
--   is a list of pushed streams... they will be pushed to the client.
data PrincipalStream = PrincipalStream {
  _headers_PS              :: Headers,
  _pushedStreams_PS        :: PushedStreams,
  _dataAndConclusion_PS    :: DataAndConclusion,
  _effect_PS               :: Effect
  }

makeLenses ''PrincipalStream

-- | Main type of this library. You implement one of these for your server.
--   This is a callback that the library calls as soon as it has
--   all the headers of a request. For GET requests that's the entire request
--   basically, but for POST and PUT requests this is just before the data
--   starts arriving to the server.
--
--   It is important that you consume the data in the cases where there is an
--   input stream, otherwise the memory is lost for the duration of the request,
--   and a malicious client can use that.
--
--   Also, notice that when handling requests your worker can be interrupted with
--   an asynchronous exception of type 'StreamCancelledException', if the peer
--   cancels the stream
type AwareWorker = Request -> IO PrincipalStream

-- | A CoherentWorker is a simplified callback that you can implement to handle requests.
--  Then you can convert it to an AwareWorker with `tupledPrincipalStreamToPrincipalStream`.
type CoherentWorker =  TupledRequest -> IO TupledPrincipalStream

-- | A tuple representing the data alone that you usually need to give as a response, that
--   is, the headers in the response (including the HTTP/2 :status), any pushed streams,
--   a stream with the response data and the footers.
type TupledPrincipalStream = (Headers, PushedStreams, DataAndConclusion)

-- | A tuple representing the data alone usually needed to create a response. That is,
--   the headers (including HTTP/2 :path, :authority, etc) and maybe an input data stream
--   for requests that include it, that is, POST and PUT.
type TupledRequest = (Headers, Maybe InputDataStream)


-- | Convert between the two types of callback.
tupledPrincipalStreamToPrincipalStream :: TupledPrincipalStream -> PrincipalStream
tupledPrincipalStreamToPrincipalStream (headers, pushed_streams, data_and_conclusion) = PrincipalStream
      {
        _headers_PS = headers,
        _pushedStreams_PS = pushed_streams,
        _dataAndConclusion_PS = data_and_conclusion,
        _effect_PS = defaultEffects
      }

requestToTupledRequest ::  Request -> TupledRequest
requestToTupledRequest req =
      (req ^. headers_RQ,
       req ^. inputData_RQ )

coherentToAwareWorker :: CoherentWorker -> AwareWorker
coherentToAwareWorker w r =
    fmap tupledPrincipalStreamToPrincipalStream $ w . requestToTupledRequest $ r

-- | Gets a single header from the list
getHeaderFromFlatList :: Headers -> B.ByteString -> Maybe B.ByteString
getHeaderFromFlatList unvl bs =
    case find (\ (x,_) -> x==bs ) unvl of
        Just (_, found_value)  -> Just found_value

        Nothing                -> Nothing


-- | If you want to skip the footers, i.e., they are empty, use this
--   function to convert an ordinary Source to a DataAndConclusion.
nullFooter :: Source IO B.ByteString -> DataAndConclusion
nullFooter s = s =$= go
  where
    go = do
        i <- await
        case i of
            Nothing ->
                return []

            Just ii -> do
                yield ii
                go
