{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings, GADTs, DeriveGeneric #-}
{- | Configuration and settings for the server. All constructor names are
     exported, but notice that they start with an underscore.
     They also have an equivalent lens without the
     underscore. Please prefer to use the lens interface.
-}
module SecondTransfer.Sessions.Config(
                 sessionId
               , defaultSessionsConfig
               , defaultSessionsEnrichedHeaders
               , sessionsCallbacks
               , sessionsEnrichedHeaders

               , reportErrorCallback_SC
               , dataDeliveryCallback_SC
               , newSessionCallback_SC
               , situationCallback_SC
               , setSoftHardClose_SC

               , dataFrameSize
               , addUsedProtocol
               , pushEnabled
               , firstPushStream
               , trayMaxSize
               , collectLatencyData
               , maxConcurrentStreams
               , sessionCloseTimeouts

               , maxWait_SCT
               , smallWait_SCT

               , SessionComponent                       (..)
               , SessionCoordinates                     (..)
               , SessionsCallbacks                      (..)
               , SessionsEnrichedHeaders                (..)
               , SessionCloseTimeouts                   (..)
               , SessionsConfig                         (..)
               , SessionGenericHandle                   (..)
               , SituationWithClient                    (..)

               , ErrorCallback
               , DataFrameDeliveryCallback
               , SituationCallback
               , NewSessionCallback                     (..)
               , HashableSockAddr                       (..)

               , ActivityMeteredSession                 (..)
               , CleanlyPrunableSession                 (..)
               , HasConnectionId                        (..)
               , EagernessToDrop                        (..)
       ) where


import           Control.Concurrent.MVar                  (MVar)
import           Control.Exception                        (SomeException)
import           Control.Lens                             (makeLenses)

import           Data.Int                                 (Int64)

import qualified Data.ByteString                          as B

--import           GHC.Generics (Generic)
--import           Data.Word                                (Word8)

--import           System.Mem.Weak                          (Weak)
import           System.Clock                             (TimeSpec)

import           SecondTransfer.IOCallbacks.Types         (IOCallbacks, ConnectionId(..))
import           SecondTransfer.Sessions.HashableSockAddr (HashableSockAddr (..))
import           SecondTransfer.MainLoop.CoherentWorker   (Http2PerceivedPriority)


-- | Eagerness to drop a connection
data EagernessToDrop =
    LetItBe_ETD   -- ^ Connection can remain open for as long as it wishes.
  | DropIt_ETD    -- ^ Drop it as soon as it becomes inactive
  | DropItNow_ETD -- ^ Drop it urgently


type SetSoftHardCloseCallback = ConnectionId -> EagernessToDrop -> IO ()


-- | Information used to identify a particular session.
newtype SessionCoordinates = SessionCoordinates ConnectionId
    deriving Show

instance Eq SessionCoordinates where
    (SessionCoordinates a) == (SessionCoordinates b) =  a == b

-- | Get/set a ConnectionId from a `SessionCoordinates`. For example, to
--   get the session id with this, import `Control.Lens.(^.)` and then do
--
-- @
--      session_id = session_coordinates ^. sessionId
-- @
--
sessionId :: Functor f => (ConnectionId -> f ConnectionId) -> SessionCoordinates -> f SessionCoordinates
sessionId f (SessionCoordinates session_id) =
    fmap SessionCoordinates (f session_id)


-- | Components at an individual session. Used to report
--   where in the session an error was produced. This interface is likely
--   to change in the future, as we add more metadata to exceptions
data SessionComponent =
    SessionInputThread_HTTP2SessionComponent
    |SessionHeadersOutputThread_HTTP2SessionComponent
    |SessionDataOutputThread_HTTP2SessionComponent
    |SessionClientPollThread_HTTP2SessionComponent
    |Framer_HTTP2SessionComponent
    |FrontendHTTP1SyntaxError_SWC -- ^ Something when talking to the browser
    |BackendHTTP1SyntaxError_SWC  -- ^ Somethint when talking to the backend application
    |Session_HTTP11
    deriving Show



-- | Used by this session engine to report an error at some component, in a particular
--   session. This callback is/was used mainly during component tests, not on
--   actual production use.
type ErrorCallback = (SessionComponent, SessionCoordinates, SomeException) -> IO ()

-- | Only used for HTTP/2 connections.
type GlobalStreamId = Int

-- | When something happens in a client...
data SituationWithClient =
    PeerReportsProtocolError_SWC
   |PeerErrored_SWC String    -- ^ Second argument gives an idea of the location
   |ConnectionCloseReceived_SWC
   |StreamResetReceived_SWC (GlobalStreamId, Maybe B.ByteString)
   |Http2DynamicHeaderTableChanged_SWC Int   -- ^ Argument is the new size
   |StreamLimitSurpassed_SWC Int -- ^ Client is trying to create too many streams, connection was closed
   |PriorityFrameReceived_SWC (GlobalStreamId, Http2PerceivedPriority) -- ^ A PRIORITY frame was received. First argument is the
                                                                       --   stream id of the receiving frame
   |PauseDueToHTTP2FlowControl_SWC
   |PreemptingConnection_SWC -- ^ Due to some internal policty, we decided to close this connection
   |UnknownFrame_SWC -- Not an error
    deriving (Show, Eq)


-- | This is the reporting callback used in actual production use.
--   First argument is the SessionId of the session where the
--   issue is being reported
type SituationCallback = ConnectionId -> SituationWithClient -> IO ()


-- | Sessions follow this class, so that they can be rescinded on inactivity
class ActivityMeteredSession a where
    sessionLastActivity :: a -> IO TimeSpec


-- | Clean-cut of sessions
class CleanlyPrunableSession a where
    cleanlyCloseSession :: a -> IO ()


-- | Allows us to get a unique, ever-increasing id for the session
class HasConnectionId a where
    getConnectionId :: a -> ConnectionId


-- | Used by the session engine to report delivery of each data frame. Keep this callback very
--   light, it runs in the main sending thread. It is called as
--   f session_id stream_id ordinal when_delivered
type DataFrameDeliveryCallback =  ConnectionId -> GlobalStreamId -> Int -> TimeSpec ->  IO ()


-- | An object that allows access to a new session
data SessionGenericHandle where
    Whole_SGH :: (ActivityMeteredSession a, CleanlyPrunableSession a, HasConnectionId a) => a -> SessionGenericHandle


instance ActivityMeteredSession SessionGenericHandle where
    sessionLastActivity (Whole_SGH a) = sessionLastActivity a


-- | Callback to be invoked when a  client establishes a new session. The first parameter
--   is the address of the client, and the second parameter is a controller that can be used to
--   reduce the number of connections from time to time, the third parameter is a key on which
--   the second paramter should be made a weak pointer
newtype NewSessionCallback =  NewSessionCallback ( HashableSockAddr -> SessionGenericHandle -> MVar Bool -> IO () )

-- | Callbacks that you can provide your sessions to notify you
--   of interesting things happening in the server.
data SessionsCallbacks = SessionsCallbacks {
    -- | Used to report errors during this session
    _reportErrorCallback_SC  :: Maybe ErrorCallback
    -- | "Ordinary" situations
 ,  _situationCallback_SC :: Maybe SituationCallback
    -- | Used to report delivery of individual data frames
  , _dataDeliveryCallback_SC :: Maybe DataFrameDeliveryCallback
    -- | Used to notify the session manager of new sessions, so
    --   that the session manager registers them (in a weak map)
    --   if need comes
  , _newSessionCallback_SC  :: Maybe NewSessionCallback
    -- | Used to ask for  soft/hard finalization
  , _setSoftHardClose_SC :: Maybe SetSoftHardCloseCallback
  }


makeLenses ''SessionsCallbacks


-- | This is a temporal interface, but an useful one nonetheless.
--   By setting some values here to True, second-transfer will add
--   some headers to inbound requests, and some headers to outbound
--   requests.
--
--   This interface is deprecated in favor of the AwareWorker
--   functionality....
data SessionsEnrichedHeaders = SessionsEnrichedHeaders {
    -- | Adds a second-transfer-eh--used-protocol header
    --   to inbound requests. Default: False
    _addUsedProtocol :: Bool
    }

makeLenses ''SessionsEnrichedHeaders

-- | Don't insert any extra-headers by default.
defaultSessionsEnrichedHeaders :: SessionsEnrichedHeaders
defaultSessionsEnrichedHeaders = SessionsEnrichedHeaders {
    _addUsedProtocol = False
    }


-- | Configuration data about acceptable timeouts
data SessionCloseTimeouts = SessionCloseTimeouts {
    -- | Absolute maximum time to wait, in nanoseconds
    _maxWait_SCT              :: Int64
    -- | Small time intervals, when we do our checks and so on.
    --   Also nanoseconds
   ,_smallWait_SCT            :: Int64
    }
    deriving (Show)

makeLenses ''SessionCloseTimeouts

defaultSessionCloseTimeouts :: SessionCloseTimeouts
defaultSessionCloseTimeouts = SessionCloseTimeouts {
   -- Let's wait a max time of thirty seconds before
   -- forcibly closing the connection.
   _maxWait_SCT       = 30*1000*1000*1000
   -- Let's wait 3000ms between checks to detect an
   -- inactivity moment. Really slow network connections
   -- will need this much grace.
  ,_smallWait_SCT     = 3*1000*1000*1000
    }

-- | Configuration information you can provide to the session maker.
data SessionsConfig = SessionsConfig {
   -- | Session callbacks
   _sessionsCallbacks         :: SessionsCallbacks
 , _sessionsEnrichedHeaders   :: SessionsEnrichedHeaders
   -- | Size to use when splitting data in data frames, to be sent.
   --   TODO: An equivalent maxRecvSize should be defined here...
 , _dataFrameSize             :: Int
   -- | Should we enable PUSH in sessions? Notice that the client
   --   can still disable PUSH at will. Also users of the library
   --   can decide not to use it. This just puts another layer ...
 , _pushEnabled               :: Bool
   -- | The number to use for the first pushed stream. Should be even
 , _firstPushStream            :: Int
   -- | Max number of packets to hold in the output tray. A high number means
   --   that more memory is used by the packets have a higher chance of going
   --   out in a favourable order.
 , _trayMaxSize               :: Int
   -- | Should we collect latency data?
 , _collectLatencyData        :: Bool
   -- | The highest stream count
 , _maxConcurrentStreams       :: Int
    -- | The gentle close-connection timeouts
 , _sessionCloseTimeouts       :: SessionCloseTimeouts
   }

makeLenses ''SessionsConfig

-- -- | Lens to access sessionsCallbacks in the `SessionsConfig` object.
-- sessionsCallbacks :: Lens' SessionsConfig SessionsCallbacks
-- sessionsCallbacks  f (
--     SessionsConfig {
--         _sessionsCallbacks= s
--     }) = fmap (\ s' -> SessionsConfig {_sessionsCallbacks = s'}) (f s)


-- | Creates a default sessions context. Modify as needed using
--   the lenses interfaces
defaultSessionsConfig :: SessionsConfig
defaultSessionsConfig = SessionsConfig {
     _sessionsCallbacks = SessionsCallbacks {
            _reportErrorCallback_SC = Nothing,
            _dataDeliveryCallback_SC = Nothing,
            _situationCallback_SC = Nothing,
            _newSessionCallback_SC = Nothing,
            _setSoftHardClose_SC = Nothing
        }
  , _sessionsEnrichedHeaders = defaultSessionsEnrichedHeaders
  , _dataFrameSize = 16*1024
  , _pushEnabled = True
  , _firstPushStream = 8
  -- | Max number of packets that can be waiting to exit. A big number here
  -- is desirable, but counts towards the memory consumption when the network is slow...
  -- so for now let's make it a little bit lower. In the future we may want to adjust
  -- this number dynamically.
  -- , _trayMaxSize = 128
  , _trayMaxSize = 16
  , _collectLatencyData = False
  , _maxConcurrentStreams = 100
  , _sessionCloseTimeouts = defaultSessionCloseTimeouts
    }
