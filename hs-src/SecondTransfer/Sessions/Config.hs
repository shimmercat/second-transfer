{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings #-}
module SecondTransfer.Sessions.Config(
    sessionId
    ,defaultSessionsConfig
    ,sessionsCallbacks
    ,reportErrorCallback


    ,SessionComponent(..)
    ,SessionCoordinates(..)
    ,SessionsCallbacks(..)
    ,SessionsConfig(..)
    ,ErrorCallback
    ) where 


-- import           Control.Concurrent.MVar (MVar)
import           Control.Exception       (SomeException)
import           Control.Lens            (Lens', makeLenses)


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
    fmap (\ s' -> (SessionCoordinates s')) (f session_id)



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



-- | Used by this session engine to report an error at some component, in a particular
--   session. 
type ErrorCallback = (SessionComponent, SessionCoordinates, SomeException) -> IO ()

-- | Callbacks that you can provide your sessions to notify you 
--   of interesting things happening in the server. 
data SessionsCallbacks = SessionsCallbacks {
    -- Callback used to report errors during this session
    _reportErrorCallback :: Maybe ErrorCallback
}

makeLenses ''SessionsCallbacks


-- | Configuration information you can provide to the session maker.
data SessionsConfig = SessionsConfig {
    -- | Session callbacks
    _sessionsCallbacks :: SessionsCallbacks
}

-- makeLenses ''SessionsConfig

-- | Lens to access sessionsCallbacks in the `SessionsConfig` object.
sessionsCallbacks :: Lens' SessionsConfig SessionsCallbacks
sessionsCallbacks  f (
    SessionsConfig {
        _sessionsCallbacks= s 
    }) = fmap (\ s' -> SessionsConfig {_sessionsCallbacks = s'}) (f s)


-- | Creates a default sessions context. Modify as needed using 
--   the lenses interfaces
defaultSessionsConfig :: SessionsConfig
defaultSessionsConfig = SessionsConfig {
    _sessionsCallbacks = SessionsCallbacks {
            _reportErrorCallback = Nothing
        }
    }

