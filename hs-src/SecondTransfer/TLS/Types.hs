{-# LANGUAGE Rank2Types, FunctionalDependencies, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module SecondTransfer.TLS.Types (
                 FinishRequest                                             (..)
               , ProtocolSelector


               , TLSContext                                                (..)

               , ConnectionId                                              (..)
               , ConnectionEvent                                           (..)
               , ConnectionCallbacks                                       (..)
               , logEvents_CoCa
               , defaultConnectionCallbacks
       ) where


import           Control.Lens
import qualified Data.ByteString                                           as B
import           Data.Int                                                  (Int64)

import qualified Network.Socket                                            as NS(SockAddr)

import           SecondTransfer.IOCallbacks.Types                          (TLSServerIO, IOChannels)

-- | Singleton type. Used in conjunction with an `MVar`. If the MVar is full,
--   the fuction `tlsServeWithALPNAndFinishOnRequest` knows that it should finish
--   at its earliest convenience and call the `CloseAction` for any open sessions.
data FinishRequest = FinishRequest

-- | Given a list of ALPN identifiers, if something is suitable, return it.
type ProtocolSelector = [B.ByteString] -> IO (Maybe Int)

-- | Simple alias to identify that something is a FilePath
type RawFilePath = B.ByteString

-- | Types implementing this class are able to say a little bit about their peer.
class IOChannels session => TLSContext ctx session | ctx -> session, session -> ctx where
    newTLSContext :: RawFilePath -> RawFilePath -> ProtocolSelector -> IO ctx
    unencryptTLSServerIO :: forall cipherio . TLSServerIO cipherio => ctx -> cipherio -> IO session
    getSelectedProtocol :: session -> IO (Maybe (Int, B.ByteString))


-- | A connection number
newtype ConnectionId = ConnectionId Int64
    deriving (Eq, Ord, Show, Enum)


-- | Connection events
data ConnectionEvent =
    -- ^ The second member says how many live connections are now
    Established_CoEv NS.SockAddr ConnectionId Int64
  | ALPNFailed_CoEv ConnectionId
  | Ended_CoEv ConnectionId


type LogCallback =  ConnectionEvent -> IO ()

-- | Callbacks used by  client applications to get notified about interesting
--   events happening at a connection level, or to get asked about things
--   (e.g, about if it is proper to accept a connection). These are used from CoreServer
data ConnectionCallbacks = ConnectionCallbacks {
    -- | Invoked after the connection is accepted, and after it is finished.
    _logEvents_CoCa            :: Maybe LogCallback
    }

makeLenses ''ConnectionCallbacks

defaultConnectionCallbacks :: ConnectionCallbacks
defaultConnectionCallbacks = ConnectionCallbacks {
    _logEvents_CoCa = Nothing
    }
