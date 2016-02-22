{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.Socks5.Types (
               -- ** Types to be serialized and de-serialized
                 ClientAuthMethods_Packet                           (..)
               , ServerSelectsMethod_Packet                         (..)
               , ClientRequest_Packet                               (..)
               , cmd_SP3
               , address_SP3
               , port_SP3
               , ServerReply_Packet                                 (..)
               , replyField_SP4
               , address_SP4
               , port_SP4
               , AddressType                                        (..)
               , ProtocolCommand                                    (..)
               , ProtocolVersion                                    (..)
               , IndicatedAddress                                   (..)
               , ReplyField                                         (..)
               -- *** Helpers for serialization, de-serialization
               , iaToAddressType

               -- *** Logging types
               , S5ConnectionId                                     (..)
               , Socks5ConnectEvent                                 (..)
               , Socks5LogCallback
               , Socks5ConnectionCallbacks                          (..)
               , logEvents_S5CC
               , blanketPlainTextIO_S5CC
     ) where

---import           Control.Concurrent
-- import qualified Control.Exception                                  as E
import           Control.Lens                                       (makeLenses)

import qualified Data.ByteString                                    as B
-- import qualified Data.ByteString.Lazy                               as LB
--import           Data.ByteString.Char8                              (pack, unpack)
--import           Data.List                                          (find)

import           Data.Word
import           Data.Int                                           (Int64)

import qualified Network.Socket                                     as NS(SockAddr)

import           SecondTransfer.IOCallbacks.Types                          (IOCallbacks)

-------------------------------------------------------------------------------------------------------
--
-- Connection callbacks, used to report Connect behavior
--
------------------------------------------------------------------------------------------------------

-- | Connection ID for SOCKS5 Connections
newtype S5ConnectionId = S5ConnectionId Int64
    deriving (Eq, Ord, Show, Enum)

-- | SOCKS5 Connections, and where are they handled
data Socks5ConnectEvent =
    Established_S5Ev NS.SockAddr S5ConnectionId
  | HandlingHere_S5Ev B.ByteString S5ConnectionId
  | ToExternal_S5Ev B.ByteString Word16  S5ConnectionId
  | Dropped_S5Ev B.ByteString  S5ConnectionId

type Socks5LogCallback =  Socks5ConnectEvent -> IO ()

-- | Callbacks used by  client applications to get notified about interesting
--   events happening at a connection level, or to get asked about things
--   (e.g, about if it is proper to accept a connection). These are used from CoreServer
data Socks5ConnectionCallbacks = Socks5ConnectionCallbacks {
    -- | Invoked after the connection is accepted, and after it is finished.
    _logEvents_S5CC            :: Maybe Socks5LogCallback

    -- | Function to transform the plain-text IOCallbacks when a connection is
    --   accepted. Handy for implementing metrics, or for slowing things down.
    --
 ,  _blanketPlainTextIO_S5CC   :: Maybe (IOCallbacks -> IO IOCallbacks)
    }

makeLenses ''Socks5ConnectionCallbacks
-------------------------------------------------------------------------------------------------------
--
--  Serialization types
--
-------------------------------------------------------------------------------------------------------

-- | A nonce which is always equal to five... or we are in troubles
data ProtocolVersion = ProtocolVersion
    deriving Show

data AddressType =
    IPv4_S5AT
  | DomainName_S5AT
  | IPv6_S5AT
    deriving (Show, Eq, Enum)


-- | The command sent by the client
data ProtocolCommand =
    Connect_S5PC
  | Bind_S5PC
  | UdpAssociate_S5PC
    deriving (Show, Eq, Enum)

-- TODO: We need to implement the other types of responses
data  ReplyField =
    Succeeded_S5RF
  | GeneralFailure_S5RF
    deriving (Show, Eq, Enum)

-- Datatypes present at different negotiation stages
data ClientAuthMethods_Packet = ClientAuthMethods_Packet {
    _version_SP1                 :: ProtocolVersion
  , _methods_SP1                 :: B.ByteString
    }
    deriving Show


data ServerSelectsMethod_Packet = ServerSelectsMethod_Packet {
    _version_SP2                 :: ProtocolVersion
  , _method_SP2                  :: Word8
    }
    deriving Show

data IndicatedAddress =
    IPv4_IA  Word32
  | DomainName_IA B.ByteString
  | IPv6_IA  B.ByteString
    deriving Show

iaToAddressType :: IndicatedAddress -> AddressType
iaToAddressType (IPv4_IA _)  = IPv4_S5AT
iaToAddressType (IPv6_IA _)  = IPv6_S5AT
iaToAddressType (DomainName_IA _) = DomainName_S5AT


data ClientRequest_Packet = ClientRequest_Packet {
    _version_SP3                 :: ProtocolVersion
  , _cmd_SP3                     :: ProtocolCommand
  , _reserved_SP3                :: Word8   -- Should be zero
  , _address_SP3                 :: IndicatedAddress
  , _port_SP3                    :: Word16
    }
    deriving Show

makeLenses ''ClientRequest_Packet


data ServerReply_Packet = ServerReply_Packet {
    _version_SP4                 :: ProtocolVersion
  , _replyField_SP4              :: ReplyField
  , _reservedField_SP4           :: Word8 -- Should be zero
  , _address_SP4                 :: IndicatedAddress
  , _port_SP4                    :: Word16
    }
    deriving Show

makeLenses ''ServerReply_Packet
