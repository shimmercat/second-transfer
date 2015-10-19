{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module SecondTransfer.TLS.Botan (
                  BotanTLSContext
                , unencryptChannelData
                , newBotanTLSContext
       ) where

import           Control.Monad                                             (when)
import           Control.Concurrent
import           Control.Concurrent.MVar

import           Foreign
import           Foreign.C.Types                                           (CChar)
import           Foreign.C.String                                          (CString)

import           Data.IORef
import qualified Data.ByteString                                           as B
import qualified Data.ByteString.Builder                                   as Bu
import qualified Data.ByteString.Lazy                                      as LB
import qualified Data.ByteString.Unsafe                                    as Un

import           Control.Lens                                              ( (^.), makeLenses, set )

-- Import all of it!
import           SecondTransfer.MainLoop.PushPullType
import           SecondTransfer.MainLoop.Protocol                          ( HttpProtocolVersion(..) )

data BotanTLSChannel

type RawFilePath = B.ByteString

type BotanTLSChannelPtr = Ptr BotanTLSChannel

type BotanTLSChannelFPtr = ForeignPtr BotanTLSChannel

data BotanSessionState =
    WaitingHandshake_BSS
  | Functioning_BSS
  | Closed_BSS


data BotanPad = BotanPad {
    _encryptedSide_BP    :: IOCallbacks
  , _availableData_BP    :: Bu.Builder
  , _sessionState_BP     :: BotanSessionState
  , _tlsChannel_BP       :: BotanTLSChannelFPtr
  , _dataCame_BP         :: MVar ()
  , _selectedProtocol_BP :: HttpProtocolVersion
  , _selfRef_BP          :: BotanPadRef
    }

type BotanPadRef = StablePtr (MVar BotanPad)

makeLenses ''BotanPad

newtype BotanSession = BotanSession (MVar BotanPad)

data BotanTLSContextAbstract

type BotanTLSContextPtr = Ptr BotanTLSContextAbstract

newtype BotanTLSContext = BotanTLSContext (ForeignPtr BotanTLSContextAbstract)

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


foreign export ccall iocba_set_protocol :: BotanPadRef -> Int -> IO ()
iocba_set_protocol siocb prt = withBotanPad siocb $ \ botan_pad -> do
    let
        used_protocol = toEnum prt
        new_botan_pad = set selectedProtocol_BP used_protocol botan_pad
    return new_botan_pad


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

foreign import ccall "&iocba_delete_tls_context" iocba_delete_tls_context :: FunPtr( BotanTLSContextPtr -> IO () )

foreign import ccall iocba_make_tls_context :: CString -> CString -> IO BotanTLSContextPtr

foreign import ccall "&iocba_delete_tls_server_channel" iocba_delete_tls_server_channel :: FunPtr( BotanTLSChannelPtr -> IO () )

foreign import ccall "wrapper" mkTlsServerDeleter :: (BotanTLSChannelPtr -> IO ()) -> IO (FunPtr ( BotanTLSChannelPtr -> IO () ) )


botanPushData :: MVar BotanPad -> LB.ByteString -> IO ()
botanPushData botan_pad_mvar datum = withMVar botan_pad_mvar $ \botan_pad -> do
    let
        strict_datum = LB.toStrict datum
        channel = botan_pad ^. tlsChannel_BP
    Un.unsafeUseAsCStringLen strict_datum $ \ (pch, len) -> do
      withForeignPtr channel $ \ c ->
         iocba_cleartext_push c pch len


-- Bad things will happen if serveral threads are calling this concurrently!!
pullAvailableData :: MVar BotanPad -> Bool -> IO B.ByteString
pullAvailableData botan_pad_mvar can_wait = do
    data_came_mvar <- withMVar botan_pad_mvar ( \ botan_pad -> return (botan_pad ^. dataCame_BP ))
    -- Just wake up
    let
        -- And here inside cleanup. The block below doesn't compose with other instances of
        -- reads happening simultaneously
        proceedToPull =
            modifyMVar botan_pad_mvar $ \botan_pad -> do
                -- Good for a deadlock
                takeMVar data_came_mvar
                let
                    new_botan_pad = set availableData_BP mempty botan_pad
                    datum = LB.toStrict . Bu.toLazyByteString . (^. availableData_BP) $ botan_pad
                return (new_botan_pad, datum)
    if can_wait
      then do
        readMVar data_came_mvar
        proceedToPull
      else do
        maybe_ok <- tryReadMVar data_came_mvar
        case maybe_ok of
            Just _ -> proceedToPull
            Nothing -> return mempty



pullCountOfData :: MVar BotanPad -> Int -> IO B.ByteString
pullCountOfData botan_pad_mvar cnt = do
    data_came_mvar <- withMVar botan_pad_mvar ( \botan_pad -> return (botan_pad ^. dataCame_BP ))
    bu <- consume data_came_mvar (fromIntegral cnt) mempty
    return . LB.toStrict . Bu.toLazyByteString $ bu
  where
    consume :: MVar () ->Int64 -> Bu.Builder -> IO Bu.Builder
    consume data_came_mvar  n gotten = do
        readMVar data_came_mvar  -- <-- Important
        -- Is there data there? Again, this only works if all reads are sequential and not
        -- two of them go together
        (ng, nn) <- modifyMVar botan_pad_mvar $ \botan_pad -> do
            -- Good for a deadlock, but since the readMVar above succeed, it should not block here
            takeMVar data_came_mvar
            let
               available_bu = (botan_pad ^. availableData_BP)
               datum = Bu.toLazyByteString available_bu
               len_datum :: Int64
               len_datum = fromIntegral . LB.length $ datum
               (set_back_bu, use, new_n)   |   len_datum <= n       =  (mempty, available_bu, n - len_datum)
                                           |   otherwise            =  ( Bu.lazyByteString . LB.drop (len_datum - n) $ datum
                                                                       , Bu.lazyByteString . LB.take n $ datum
                                                                       , 0 )
               new_botan_pad = set availableData_BP set_back_bu botan_pad
               new_gotten = gotten `mappend` use
            return (new_botan_pad, (new_gotten, new_n))
        if nn == 0
            then return ng
        else
            consume data_came_mvar nn ng


foreign import ccall iocba_new_tls_server_channel ::
     BotanPadRef
     -> BotanTLSContextPtr
     -> Int          -- protocol selection policty
     -> IO BotanTLSChannelPtr


newBotanTLSContext :: RawFilePath -> RawFilePath ->  IO BotanTLSContext
newBotanTLSContext cert_filename privkey_filename = do
    B.useAsCString cert_filename $ \ s1 ->
        B.useAsCString privkey_filename $ \ s2 -> do
            ctx_ptr <-  iocba_make_tls_context s1 s2
            x <- newForeignPtr iocba_delete_tls_context ctx_ptr
            return $ BotanTLSContext x


unencryptChannelData :: TLSServerIO a =>  BotanTLSContext -> a -> IO BotanSession
unencryptChannelData botan_ctx@(BotanTLSContext fctx) tls_data  = do
    botan_pad_mvar <- newEmptyMVar
    botan_pad_stable_ref <- newStablePtr botan_pad_mvar


    tls_channel_ptr <- withForeignPtr fctx $ \ x -> iocba_new_tls_server_channel botan_pad_stable_ref x 1
    tls_channel_fptr <- newForeignPtr iocba_delete_tls_server_channel tls_channel_ptr
    tls_io_callbacks <- handshake tls_data
    data_came_mvar <- newEmptyMVar

    let
        new_botan_pad = BotanPad {
            _encryptedSide_BP     = tls_io_callbacks
          , _availableData_BP     = mempty
          , _sessionState_BP      = WaitingHandshake_BSS
          , _tlsChannel_BP        = tls_channel_fptr
          , _dataCame_BP          = data_came_mvar
          , _selectedProtocol_BP  = Http11_HPV -- Filler
          , _selfRef_BP           = botan_pad_stable_ref
          }

    modifyMVar_ botan_pad_mvar (\ _ -> return new_botan_pad )
    mkWeakMVar botan_pad_mvar $ do
        freeStablePtr botan_pad_stable_ref

    return $ BotanSession botan_pad_mvar


instance IOChannels BotanSession where
    handshake botan_session@(BotanSession botan_pad_mvar) = do
        let
            push_action :: PushAction
            push_action bs = botanPushData botan_pad_mvar bs

            pull_action :: PullAction
            pull_action = pullCountOfData botan_pad_mvar

            best_effort_pull_action :: BestEffortPullAction
            best_effort_pull_action = pullAvailableData botan_pad_mvar

            -- TODO: Implement me!!!!!!!
            close_action :: CloseAction
            close_action = return ()
        return $ IOCallbacks {
            _pushAction_IOC = push_action
          , _pullAction_IOC = pull_action
          , _bestEffortPullAction_IOC = best_effort_pull_action
          , _closeAction_IOC = close_action
            }
