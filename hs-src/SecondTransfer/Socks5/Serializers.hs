{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.Socks5.Serializers (
                 putProtocolVersion
               , putReplyField
               , putServerSelectsMethod_Packet
               , putServerReply_Packet
     ) where

--import           Control.Applicative                                ( (<|>) )

--import qualified Data.Binary.Builder                                as U
import qualified Data.Binary                                        as U
import qualified Data.Binary.Put                                    as U

--import           Data.Word
--import           Data.Bits
import qualified Data.ByteString                                    as B
--import qualified Data.Attoparsec.ByteString                         as P

import           SecondTransfer.Socks5.Types


putProtocolVersion :: ProtocolVersion -> U.Put
putProtocolVersion _ = U.putWord8 5


putReplyField :: ReplyField -> U.Put
putReplyField Succeeded_S5RF = U.putWord8 0
putReplyField GeneralFailure_S5RF = U.putWord8 1


putServerSelectsMethod_Packet :: ServerSelectsMethod_Packet -> U.Put
putServerSelectsMethod_Packet (ServerSelectsMethod_Packet _v m) =
    putProtocolVersion _v >> (U.putWord8 . fromIntegral $  m)


putAddressType :: AddressType -> U.Put
putAddressType IPv4_S5AT = U.putWord8 1
putAddressType DomainName_S5AT = U.putWord8 3
putAddressType IPv6_S5AT = U.putWord8 4


putIndicatedAddress :: IndicatedAddress -> U.Put
putIndicatedAddress (IPv4_IA w) =  U.putWord32be w
putIndicatedAddress (DomainName_IA dn) =
    (U.putWord8 . fromIntegral $  B.length dn)
 >> U.putByteString dn
putIndicatedAddress (IPv6_IA s) = U.putByteString s


putServerReply_Packet :: ServerReply_Packet -> U.Put
putServerReply_Packet (ServerReply_Packet _version reply_field _reserved address port) =
    do
        putProtocolVersion _version
        putReplyField reply_field
        U.putWord8 0 -- reserved
        putAddressType $ iaToAddressType address
        putIndicatedAddress address
        U.putWord16be port
