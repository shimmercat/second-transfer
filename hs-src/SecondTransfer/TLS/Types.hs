{-# LANGUAGE Rank2Types, FunctionalDependencies #-}
module SecondTransfer.TLS.Types (
                 FinishRequest                                             (..)
               , ProtocolSelector


               , TLSContext                                                (..)
       ) where

import qualified Data.ByteString                                           as B

import           SecondTransfer.IOCallbacks.Types                          (TLSServerIO, IOChannels)

-- | Singleton type. Used in conjunction with an `MVar`. If the MVar is full,
--   the fuction `tlsServeWithALPNAndFinishOnRequest` knows that it should finish
--   at its earliest convenience and call the `CloseAction` for any open sessions.
data FinishRequest = FinishRequest

type ProtocolSelector = [B.ByteString] -> IO (Maybe Int)

type RawFilePath = B.ByteString


class IOChannels session => TLSContext ctx session | ctx -> session, session -> ctx where
    newTLSContext :: RawFilePath -> RawFilePath -> ProtocolSelector -> IO ctx
    unencryptTLSServerIO :: forall cipherio . TLSServerIO cipherio => ctx -> cipherio -> IO session
    getSelectedProtocol :: session -> IO (Maybe (Int, B.ByteString))
