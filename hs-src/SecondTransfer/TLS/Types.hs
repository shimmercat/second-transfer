{-# LANGUAGE Rank2Types, FunctionalDependencies, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module SecondTransfer.TLS.Types (
                 FinishRequest                                             (..)
--               , ProtocolSelector


               , TLSContext                                                (..)

               , ConnectionId                                              (..)
               , ConnectionEvent                                           (..)

               , ConnectionCallbacks                                       (..)

               , ConnectionTransformCallback
               , LogCallback
               , ServiceIsClosingCallback

               , logEvents_CoCa
               , blanketPlainTextIO_CoCa
               , serviceIsClosing_CoCa

               , defaultConnectionCallbacks

               , TLSSessionStorage                                          (..)
       ) where


import           Control.Lens
import qualified Data.ByteString                                           as B
import           Data.Int                                                  (Int64)

import qualified Network.Socket                                            as NS(SockAddr)

import           SecondTransfer.MainLoop.Protocol
import           SecondTransfer.IOCallbacks.Types                          (
                                                                           TLSServerIO,
                                                                           IOChannels,
                                                                           IOCallbacks,
                                                                           ConnectionData,
                                                                           ConnectionId (..)
                                                                           )

import          SecondTransfer.IOCallbacks.WrapSocket

import          SecondTransfer.TLS.SessionStorage

-- | Singleton type. Used in conjunction with an `MVar`. If the MVar is full,
--   the fuction `tlsServeWithALPNAndFinishOnRequest` knows that it should finish
--   at its earliest convenience and call the `CloseAction` for any open sessions.
data FinishRequest = FinishRequest


-- type ProtocolSelector = [B.ByteString] -> IO (Maybe Int)


-- | Class to have different kinds of TLS backends. Included here and enabled through 'enable-botan'
--   is support for using Botan as a backend. HTTP/2 requires TLS 1.2 and ALPN, so older versions
--   of many TLS libraries are not suitable.
--
--
class IOChannels session => TLSContext ctx session | ctx -> session, session -> ctx where
    newTLSContextFromMemory :: B.ByteString -> B.ByteString -> HttpProtocolVersion -> IO ctx
    -- ^  /newTLSContextFromMemory cert_data key_data protocol_selector/ creates a new context, provided
    -- certificate data. The certificate data must be in X509 format. The private key should be in PKCS8 format
    -- /without/ password.
    newTLSContextFromCertFileNames :: B.ByteString -> B.ByteString -> HttpProtocolVersion -> IO ctx -- ^ newTLSContextFromMemory cert_filename key_filename protocol_selector
    -- ^ Same as before, but using filename instead of certificates loaded into memory.
    unencryptTLSServerIO :: forall cipherio . TLSServerIO cipherio => ctx -> cipherio -> IO session
    -- ^ Returns the protocoll finally selected for a session.
    getSelectedProtocol :: session -> IO HttpProtocolVersion
    -- ^ Some TLS implementations can use session resumption. This returns true if the
    --   enabling is successfull
    enableSessionResumption :: forall a . TLSSessionStorage a =>  ctx -> a -> IO Bool
    enableSessionResumption _ _ = return False
    -- ^ Says if a session was resumed. For backends without support for session resumption,
    --   this always returns False. Notice that this call may block if the information
    --   about the session's resumption status is not yet in memory.
    sessionWasResumed :: session -> IO Bool
    sessionWasResumed _ = return False

-- | Connection events
data ConnectionEvent =
    Established_CoEv NS.SockAddr ConnectionId Int64        -- ^ New connection. The second member says how many live connections are now
  | ALPNFailed_CoEv ConnectionId                           -- ^ An ALPN negotiation failed
  | Ended_CoEv ConnectionId                                -- ^ A connection ended.
  | TooManyInHandshake_CoEv NS.SockAddr                    -- ^ Somebody was opening too many TCP connections  without
                                                           --   finishing the TLS handshake, and therefore we are
                                                           --   dropping connections from that client.
  | TLSHandshakeTimeOut_CoEv NS.SockAddr                   -- ^ When I can't finish the handshake
  | AcceptError_CoEv AcceptErrorCondition                  -- ^. A condition


-- | See the docs below
type LogCallback =  ConnectionEvent -> IO ()


-- | See below
type ConnectionTransformCallback = ConnectionData -> IOCallbacks -> IO IOCallbacks

-- | The service is closing, stop accepting connections
type ServiceIsClosingCallback = IO Bool


-- | Callbacks used by  client applications to get notified about interesting
--   events happening at a connection level, or to get asked about things
--   (e.g, about if it is proper to accept a connection). These are used from CoreServer
data ConnectionCallbacks = ConnectionCallbacks {
    -- | Invoked after the connection is accepted, and after it is finished.
    _logEvents_CoCa            :: Maybe LogCallback

    -- | Function to transform the plain-text IOCallbacks when a connection is
    --   accepted. Handy for implementing metrics, or for slowing things down.
 ,  _blanketPlainTextIO_CoCa  :: Maybe ConnectionTransformCallback

    -- | Invoked before accepting a new connection. If False, the connection
    --   is accepted. If True, the connection is rejected, the socket is
    --   closed and the thread finished
 , _serviceIsClosing_CoCa     :: Maybe ServiceIsClosingCallback
    }

makeLenses ''ConnectionCallbacks

-- | Default connections callback. Empty
defaultConnectionCallbacks :: ConnectionCallbacks
defaultConnectionCallbacks = ConnectionCallbacks {
    _logEvents_CoCa = Nothing
  , _blanketPlainTextIO_CoCa = Nothing
  , _serviceIsClosing_CoCa = Nothing
    }
