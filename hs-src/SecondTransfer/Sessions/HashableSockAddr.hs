{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings, GADTs, DeriveGeneric #-}
module SecondTransfer.Sessions.HashableSockAddr (
                 HashableSockAddr                        (..)
               , hashableSockAddrFromNSSockAddr
        ) where


import           GHC.Generics (Generic)

import           Data.Hashable
import           Data.Word

import           Network.Socket (SockAddr(..), hostAddress6ToTuple)

-- | Since SockAddr is not hashable, we need our own. TODO: IPv6.
data HashableSockAddr =
      HashableSockAddr_IpV4 (Word8, Word8, Word8, Word8)
    | HashableSockAddr_IpV6 (Word32, Word32, Word32, Word32
                                )
        deriving (Eq, Show, Generic)

instance  Hashable HashableSockAddr


hashableSockAddrFromNSSockAddr :: SockAddr -> Maybe HashableSockAddr
hashableSockAddrFromNSSockAddr (SockAddrInet _port_number haddr) =
  Just . HashableSockAddr_IpV4 $ (
    fromIntegral $ haddr `div` 16777216,
    fromIntegral $ haddr `div` 65536 `mod` 256,
    fromIntegral $ haddr `div` 256 `mod` 256,
    fromIntegral $ haddr `mod` 256
  )
hashableSockAddrFromNSSockAddr (SockAddrInet6 _ _ haddr _) =
  Just . HashableSockAddr_IpV6 $ haddr
hashableSockAddrFromNSSockAddr _ = Nothing
