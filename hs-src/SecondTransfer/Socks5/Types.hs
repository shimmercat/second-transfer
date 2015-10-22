{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.Socks5.Types (

                 ServerSocks5Stage                                  (..)
               , Socks5Resolver                                     (..)

               -- ** Types to be serialized and de-serialized
               , ClientAuthMethods_Packet                           (..)
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
     ) where

---import           Control.Concurrent
-- import qualified Control.Exception                                  as E
import           Control.Lens                                       (makeLenses, (^.))

import qualified Data.ByteString                                    as B
-- import qualified Data.ByteString.Lazy                               as LB
--import           Data.ByteString.Char8                              (pack, unpack)
--import           Data.List                                          (find)

import           Data.Word

import           SecondTransfer.IOCallbacks.Types                   (DisruptibleAttendant)

-------------------------------------------------------------------------------------------------------
--
--  Behavioral types
--
-------------------------------------------------------------------------------------------------------

-- | Resolver for Socks5. It's different than DNS, because it accepts a port number.
class Socks5Resolver a where
    -- If resolve is successfull, this returns a function that can be
    -- passed a IOCallbacks and do something with them....
    --
    --  The third argument is the port!
    s5Resolve :: a ->  IndicatedAddress -> Int -> IO (Maybe DisruptibleAttendant)

-------------------------------------------------------------------------------------------------------
--
--  Serialization types
--
-------------------------------------------------------------------------------------------------------

-- | A nonce which is always equal to five... or we are in troubles
data ProtocolVersion = ProtocolVersion


data AddressType =
    IPv4_S5AT
  | DomainName_S5AT
  | IPv6_S5AT


-- | The command sent by the client
data ProtocolCommand =
    Connect_S5PC
  | Bind_S5PC
  | UdpAssociate_S5PC


data ServerSocks5Stage =
    Ready_SS5S             -- ^ Waiting for client greeting with auth methods
  | WaitingRequest_SS5S    -- ^ Server is waiting for the Socks5 request
  | Functioning_SS5S
  | Failed_SS5S


-- TODO: We need to implement the other types of responses
data  ReplyField =
    Succeeded_S5RF
  | GeneralFailure_S5RF


-- Datatypes present at different negotiation stages
data ClientAuthMethods_Packet = ClientAuthMethods_Packet {
    _version_SP1                 :: ProtocolVersion
  , _methods_SP1                 :: B.ByteString
    }


data ServerSelectsMethod_Packet = ServerSelectsMethod_Packet {
    _version_SP2                 :: ProtocolVersion
  , _method_SP2                  :: Word8
    }

data IndicatedAddress =
    IPv4_IA  Word32
  | DomainName_IA B.ByteString
  | IPv6_IA  B.ByteString


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

makeLenses ''ClientRequest_Packet


data ServerReply_Packet = ServerReply_Packet {
    _version_SP4                 :: ProtocolVersion
  , _replyField_SP4              :: ReplyField
  , _reservedField_SP4           :: Word8 -- Should be zero
  , _address_SP4                 :: IndicatedAddress
  , _port_SP4                    :: Word16
    }

makeLenses ''ServerReply_Packet
