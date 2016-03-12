{-# LANGUAGE Rank2Types, FunctionalDependencies, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module SecondTransfer.TLS.Types (
                 FinishRequest                                             (..)
               , ProtocolSelector


               , TLSContext                                                (..)

               , ConnectionId                                              (..)
               , ConnectionEvent                                           (..)

               , ConnectionCallbacks                                       (..)
               , logEvents_CoCa
               , blanketPlainTextIO_CoCa

               , defaultConnectionCallbacks
       ) where


import           Control.Lens
import qualified Data.ByteString                                           as B
import           Data.Int                                                  (Int64)

import qualified Network.Socket                                            as NS(SockAddr)

import           SecondTransfer.IOCallbacks.Types                          (
                                                                           TLSServerIO,
                                                                           IOChannels,
                                                                           IOCallbacks,
                                                                           ConnectionData,
                                                                           ConnectionId (..)
                                                                           )

-- | Singleton type. Used in conjunction with an `MVar`. If the MVar is full,
--   the fuction `tlsServeWithALPNAndFinishOnRequest` knows that it should finish
--   at its earliest convenience and call the `CloseAction` for any open sessions.
data FinishRequest = FinishRequest

-- | Callback function to select a protocol during the ALPN negotiation phase.
--   Given a list of ALPN identifiers, if something is suitable, return it.
type ProtocolSelector = [B.ByteString] -> IO (Maybe Int)


-- | Class to have different kinds of TLS backends. Included here and enabled through 'enable-botan'
--   is support for using Botan as a backend. HTTP/2 requires TLS 1.2 and ALPN, so older versions
--   of many TLS libraries are not suitable.
--
--
class IOChannels session => TLSContext ctx session | ctx -> session, session -> ctx where
    newTLSContextFromMemory :: B.ByteString -> B.ByteString -> ProtocolSelector -> IO ctx
    -- ^  /newTLSContextFromMemory cert_data key_data protocol_selector/ creates a new context, provided
    -- certificate data. The certificate data must be in X509 format. The private key should be in PKCS8 format
    -- /without/ password.
    newTLSContextFromCertFileNames :: B.ByteString -> B.ByteString -> ProtocolSelector -> IO ctx -- ^ newTLSContextFromMemory cert_filename key_filename protocol_selector
    -- ^ Same as before, but using filename instead of certificates loaded into memory.
    unencryptTLSServerIO :: forall cipherio . TLSServerIO cipherio => ctx -> cipherio -> IO session

    -- ^ Returns the protocoll finally selected for a session.
    getSelectedProtocol :: session -> IO (Maybe (Int, B.ByteString))


-- | Connection events
data ConnectionEvent =
    Established_CoEv NS.SockAddr ConnectionId Int64        -- ^ New connection. The second member says how many live connections are now
  | ALPNFailed_CoEv ConnectionId                           -- ^ An ALPN negotiation failed
  | Ended_CoEv ConnectionId                                -- ^ A connection ended.


type LogCallback =  ConnectionEvent -> IO ()

-- | Callbacks used by  client applications to get notified about interesting
--   events happening at a connection level, or to get asked about things
--   (e.g, about if it is proper to accept a connection). These are used from CoreServer
data ConnectionCallbacks = ConnectionCallbacks {
    -- | Invoked after the connection is accepted, and after it is finished.
    _logEvents_CoCa            :: Maybe LogCallback

    -- | Function to transform the plain-text IOCallbacks when a connection is
    --   accepted. Handy for implementing metrics, or for slowing things down.
 ,  _blanketPlainTextIO_CoCa  :: Maybe (ConnectionData -> IOCallbacks -> IO IOCallbacks)
    }

makeLenses ''ConnectionCallbacks

-- | Default connections callback. Empty
defaultConnectionCallbacks :: ConnectionCallbacks
defaultConnectionCallbacks = ConnectionCallbacks {
    _logEvents_CoCa = Nothing
  , _blanketPlainTextIO_CoCa = Nothing
    }
