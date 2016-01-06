{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings, GADTs, DeriveGeneric #-}
module SecondTransfer.Sessions.HashableSockAddr (
                 HashableSockAddr
               , hashableSockAddrFromNSSockAddr
        ) where


import           GHC.Generics (Generic)

import           Data.Hashable
import           Data.Word

import           Network.Socket (SockAddr(..))

-- | Since SockAddr is not hashable, we need our own. TODO: IPv6.
newtype HashableSockAddr = HashableSockAddr (Word8, Word8, Word8, Word8)
    deriving (Eq, Show, Generic)

instance  Hashable HashableSockAddr


hashableSockAddrFromNSSockAddr :: SockAddr -> Maybe HashableSockAddr
hashableSockAddrFromNSSockAddr (SockAddrInet _port_number haddr) =
  Just . HashableSockAddr $ (
    fromIntegral $ haddr `div` 16777216,
    fromIntegral $ haddr `div` 65536 `mod` 256,
    fromIntegral $ haddr `div` 256 `mod` 256,
    fromIntegral $ haddr `mod` 256
  )
hashableSockAddrFromNSSockAddr _ = Nothing
