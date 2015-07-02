{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings #-}
{- | Configuration and settings for the server. All constructor names are
     exported, but notice that they start with an underscore.
     They also have an equivalent lens without the
     underscore. Please prefer to use the lens interface.
-}
module SecondTransfer.Sessions.Config(
    sessionId
    ,defaultSessionsConfig
    ,defaultSessionsEnrichedHeaders
    ,sessionsCallbacks
    ,sessionsEnrichedHeaders
    ,reportErrorCallback_SC
    ,dataDeliveryCallback_SC
    ,dataFrameSize
    ,addUsedProtocol


    ,SessionComponent(..)
    ,SessionCoordinates(..)
    ,SessionsCallbacks(..)
    ,SessionsEnrichedHeaders(..)
    -- ,UsedProtocol(..)
    ,SessionsConfig(..)
    ,ErrorCallback
    ) where


-- import           Control.Concurrent.MVar (MVar)
import           Control.Exception       (SomeException)
import           Control.Lens            (makeLenses)
import           System.Clock            (TimeSpec)


-- | Information used to identify a particular session.
newtype SessionCoordinates = SessionCoordinates  Int
    deriving Show

instance Eq SessionCoordinates where
    (SessionCoordinates a) == (SessionCoordinates b) =  a == b

-- | Get/set a numeric Id from a `SessionCoordinates`. For example, to
--   get the session id with this, import `Control.Lens.(^.)` and then do
--
-- @
--      session_id = session_coordinates ^. sessionId
-- @
--
sessionId :: Functor f => (Int -> f Int) -> SessionCoordinates -> f SessionCoordinates
sessionId f (SessionCoordinates session_id) =
    fmap SessionCoordinates (f session_id)


-- | Components at an individual session. Used to report
--   where in the session an error was produced. This interface is likely
--   to change in the future, as we add more metadata to exceptions
data SessionComponent =
    SessionInputThread_HTTP2SessionComponent
    |SessionHeadersOutputThread_HTTP2SessionComponent
    |SessionDataOutputThread_HTTP2SessionComponent
    |Framer_HTTP2SessionComponent
    |Session_HTTP11
    deriving Show


-- Which protocol a session is using... no need for this right now
-- data UsedProtocol =
--      HTTP11_UsP
--     |HTTP2_UsP

-- | Used by this session engine to report an error at some component, in a particular
--   session.
type ErrorCallback = (SessionComponent, SessionCoordinates, SomeException) -> IO ()

-- | Used by the session engine to report delivery of a data frame. Keep this callback very
--   light, it runs in the main sending thread. It is called as
--   f session_id stream_id ordinal when_delivered
type DataFrameDeliveryCallback =  Int -> Int -> Int -> TimeSpec ->  IO ()

-- | Callbacks that you can provide your sessions to notify you
--   of interesting things happening in the server.
data SessionsCallbacks = SessionsCallbacks {
    -- Callback used to report errors during this session
    _reportErrorCallback_SC  :: Maybe ErrorCallback,
    -- Callback used to report delivery of individual data frames
    _dataDeliveryCallback_SC :: Maybe DataFrameDeliveryCallback
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


-- | Configuration information you can provide to the session maker.
data SessionsConfig = SessionsConfig {
    -- | Session callbacks
    _sessionsCallbacks :: SessionsCallbacks
    ,_sessionsEnrichedHeaders :: SessionsEnrichedHeaders
    -- | Size to use when splitting data in data frames
    ,_dataFrameSize :: Int
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
            _dataDeliveryCallback_SC = Nothing
        },
    _sessionsEnrichedHeaders = defaultSessionsEnrichedHeaders,
    _dataFrameSize = 2048
    }
