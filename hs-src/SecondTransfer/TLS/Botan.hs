{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module SecondTransfer.TLS.Botan (
       ) where

import           Control.Monad                                             (when)
import           Control.Concurrent
import           Control.Concurrent.MVar

import           Foreign
import           Foreign.C.Types                                           (CChar)

import           Data.IORef
import qualified Data.ByteString                                           as B
import qualified Data.ByteString.Builder                                   as Bu
import qualified Data.ByteString.Lazy                                      as LB
import qualified Data.ByteString.Unsafe                                    as Un

import           Control.Lens                                              ( (^.), makeLenses, set )

-- Import all of it!
import           SecondTransfer.MainLoop.PushPullType

data BotanTLSChannel

type BotanTLSChannelPtr = Ptr BotanTLSChannel

type BotanTLSChannelFPtr = ForeignPtr BotanTLSChannel

data BotanSessionState =
    WaitingHandshake_BSS
  | Functioning_BSS
  | Closed_BSS


data BotanPad = BotanPad {
    _encryptedSide_BP :: IOCallbacks
  , _availableData_BP :: Bu.Builder
  , _sessionState_BP  :: BotanSessionState
  , _tlsChannel_BP    :: BotanTLSChannelFPtr
  , _dataCame_BP      :: MVar ()
    }

makeLenses ''BotanPad

type BotanPadRef = StablePtr (MVar BotanPad)

withBotanPad :: BotanPadRef -> (BotanPad -> IO BotanPad ) -> IO ()
withBotanPad siocb cb = deRefStablePtr siocb >>= (\ bpmvar -> modifyMVar_ bpmvar cb )

foreign export ccall iocba_push :: BotanPadRef -> Ptr CChar -> Int -> IO ()
iocba_push siocb p len = withBotanPad siocb $ \ botan_pad ->  do
    let
        push_action = botan_pad ^.  (encryptedSide_BP . pushAction_IOC )
    let cstr = (p, fromIntegral len)
    b <- B.packCStringLen cstr
    push_action (LB.fromStrict b)
    return botan_pad


foreign export ccall iocba_data_cb :: BotanPadRef -> Ptr CChar -> Int -> IO ()
iocba_data_cb siocb p len = withBotanPad siocb $ \ botan_pad -> do
    let
        cstr = (p, fromIntegral len)
        avail_data = botan_pad ^. availableData_BP
    b <- B.packCStringLen cstr
    tryPutMVar (botan_pad ^. dataCame_BP) ()
    return $ set availableData_BP (avail_data `mappend` (Bu.byteString b) ) botan_pad


-- TODO: Should we raise some sort of exception here? Maybe call the "closeAction"
-- in the other sides?
foreign export ccall iocba_alert_cb :: BotanPadRef -> Int -> IO ()
iocba_alert_cb siocb alert_code = withBotanPad siocb $ \botan_pad -> do
    let
        session_state = botan_pad ^. sessionState_BP
    return $
        if (alert_code < 0)
          then set sessionState_BP Closed_BSS botan_pad
          else botan_pad


-- Botan relies a wealth of information here, not using at the moment :-(
foreign export ccall iocba_handshake_cb :: BotanPadRef -> IO ()
iocba_handshake_cb siocb = withBotanPad siocb $ \botan_pad -> do
    let
        session_state = botan_pad ^. sessionState_BP
    tryPutMVar (botan_pad ^. dataCame_BP) ()
    return $ set sessionState_BP Functioning_BSS botan_pad


foreign import ccall iocba_cleartext_push :: BotanTLSChannelPtr -> Ptr CChar -> Int -> IO ()

botanPushData :: MVar BotanPad -> LB.ByteString -> IO ()
botanPushData botan_pad_mvar datum = withMVar botan_pad_mvar $ \botan_pad -> do
    let
        strict_datum = LB.toStrict datum
        channel = botan_pad ^. tlsChannel_BP
    Un.unsafeUseAsCStringLen strict_datum $ \ (pch, len) -> do
      withForeignPtr channel $ \ c ->
         iocba_cleartext_push c pch len


pullAvailableData :: MVar BotanPad -> IO B.ByteString
pullAvailableData botan_pad_mvar = modifyMVar botan_pad_mvar $ \botan_pad -> do
    takeMVar (botan_pad ^. dataCame_BP)
    let
        new_botan_pad = set availableData_BP mempty botan_pad
        datum = LB.toStrict . Bu.toLazyByteString . (^. availableData_BP) $ botan_pad
    return (new_botan_pad, datum)
