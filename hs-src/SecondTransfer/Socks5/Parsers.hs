{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.Socks5.Parsers (
                 parseProtocolVersion
               , parseAddressType
               , parseProtocolCommand
               , parseReplyField
               , parseClientAuthMethods_Packet
               , parseServerSelectsMethod_Packet
               , parseIndicatedAddress
               , parseClientRequest_Packet
     ) where

import           Control.Applicative                                ( (<|>) )

import           Data.Word
import           Data.Bits
import qualified Data.ByteString                                    as B
import qualified Data.Attoparsec.ByteString                         as P

import           SecondTransfer.Socks5.Types


parseProtocolVersion :: P.Parser ProtocolVersion
parseProtocolVersion = P.word8 5 >> pure  ProtocolVersion


parseAddressType :: P.Parser AddressType
parseAddressType =
       ( P.word8 1 >> pure IPv4_S5AT)
  <|>  ( P.word8 3 >> pure DomainName_S5AT)
  <|>  ( P.word8 4 >> pure IPv6_S5AT)


parseProtocolCommand :: P.Parser ProtocolCommand
parseProtocolCommand =
       ( P.word8 1 >> pure Connect_S5PC )
  <|>  ( P.word8 2 >> pure Bind_S5PC )
  <|>  ( P.word8 3 >> pure UdpAssociate_S5PC )


parseReplyField :: P.Parser ReplyField
parseReplyField =
       ( P.word8 0 >> pure Succeeded_S5RF )
  <|>  ( P.word8 1 >> pure GeneralFailure_S5RF )


parseClientAuthMethods_Packet :: P.Parser ClientAuthMethods_Packet
parseClientAuthMethods_Packet = ClientAuthMethods_Packet <$>
       parseProtocolVersion
  <*>  ( P.anyWord8 >>= \ len -> P.take . fromIntegral $ len)


parseServerSelectsMethod_Packet :: P.Parser ServerSelectsMethod_Packet
parseServerSelectsMethod_Packet = error "YetTooImplement(ButItsSuperEasy)"


parseIndicatedAddress :: AddressType -> P.Parser IndicatedAddress
parseIndicatedAddress IPv4_S5AT =
       IPv4_IA
  <$>  anyWord32be
parseIndicatedAddress IPv6_S5AT =
       IPv6_IA
  <$>  P.take 16
parseIndicatedAddress DomainName_S5AT =
       DomainName_IA
  <$>  (P.anyWord8 >>= \ len -> P.take . fromIntegral $ len)


parseClientRequest_Packet :: P.Parser ClientRequest_Packet
parseClientRequest_Packet =
       ClientRequest_Packet
  <$>  parseProtocolVersion
  <*>  parseProtocolCommand
  <*>  (P.word8 0)  -- Reserved part
  <*>  (parseAddressType >>= parseIndicatedAddress )
  <*>  anyWord16be

-- Some definitions copy-pasted from Data.Attoparsec.Binary ... Original code by Andrew Drake,
-- all rights reserved by him, used under BSD3 license.

byteSize :: (FiniteBits a) => a -> Int
byteSize = (`div` 8) . finiteBitSize

pack :: (FiniteBits a, Num a) => B.ByteString -> a
pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

anyWordN :: (FiniteBits a) => (B.ByteString -> a) -> P.Parser a
anyWordN = anyWordN' undefined
  where anyWordN' :: (FiniteBits a) => a -> (B.ByteString -> a) -> P.Parser a
        anyWordN' d = flip fmap $ P.take $ byteSize d

-- | Match any 16 bit big endian word.
anyWord16be :: P.Parser Word16
anyWord16be = anyWordN pack


-- | Match any 32 bit big endian word.
anyWord32be :: P.Parser Word32
anyWord32be = anyWordN pack


unpack :: (FiniteBits a, Integral a) => a -> B.ByteString
unpack x = B.pack $ map f $ reverse [0..byteSize x - 1]
   where f s = fromIntegral $ shiftR x (8 * s)

wordN :: (FiniteBits a) => (a -> B.ByteString) -> a -> P.Parser a
wordN u w = P.string (u w) >> return w

 -- |Match a specific 16-bit big-endian word.
word16be :: Word16 -> P.Parser Word16
word16be = wordN unpack
