{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module SecondTransfer.TLS.Botan (
                  BotanTLSContext
                , unencryptChannelData
                , newBotanTLSContext
       ) where

--import           Control.Monad                                             (when)
import           Control.Concurrent
--import           Control.Concurrent.MVar
import qualified Control.Exception                                         as E

import           Foreign
import           Foreign.C.Types                                           (CChar)
import           Foreign.C.String                                          (CString)

import           Data.List                                                 (elemIndex)
import           Data.Maybe                                                (fromMaybe)
--import           Data.IORef
import qualified Data.ByteString                                           as B
import qualified Data.ByteString.Builder                                   as Bu
import qualified Data.ByteString.Lazy                                      as LB
import qualified Data.ByteString.Unsafe                                    as Un

import           Control.Lens                                              ( (^.), makeLenses, set )

-- Import all of it!
import           SecondTransfer.MainLoop.PushPullType
--import           SecondTransfer.MainLoop.Protocol                          ( HttpProtocolVersion(..) )
import           SecondTransfer.Exception                                  ( IOProblem )

data BotanTLSChannel

type RawFilePath = B.ByteString

type BotanTLSChannelPtr = Ptr BotanTLSChannel

type BotanTLSChannelFPtr = ForeignPtr BotanTLSChannel

data BotanSessionState =
    WaitingHandshake_BSS
  | Functioning_BSS
  | Closed_BSS


data BotanPad = BotanPad {
    _encryptedSide_BP      :: IOCallbacks
  , _availableData_BP      :: Bu.Builder
  , _sessionState_BP       :: BotanSessionState
  , _tlsChannel_BP         :: BotanTLSChannelFPtr
  , _dataCame_BP           :: MVar ()
  , _handshakeCompleted_BP :: MVar ()
  , _protocolSelector_BP   :: B.ByteString -> IO (Int, B.ByteString)
  , _selectedProtocol_BP   :: (Int, B.ByteString)
  , _problem_BP            :: MVar IOProblem
    }

type BotanPadRef = StablePtr (MVar BotanPad)

makeLenses ''BotanPad

newtype BotanSession = BotanSession (MVar BotanPad)

data BotanTLSContextAbstract

type BotanTLSContextCSidePtr = Ptr BotanTLSContextAbstract

type BotanTLSContextCSideFPtr = ForeignPtr BotanTLSContextAbstract

type ProtocolSelector = [B.ByteString] -> IO B.ByteString

data BotanTLSContext = BotanTLSContext {
    _cppSide_BTC          :: BotanTLSContextCSideFPtr
  , _protocolSelector_BTC :: ProtocolSelector
    }

makeLenses ''BotanTLSContext


withBotanPad :: BotanPadRef -> (BotanPad -> IO BotanPad ) -> IO ()
withBotanPad siocb cb = deRefStablePtr siocb >>= (\ bpmvar -> modifyMVar_ bpmvar cb )

withBotanPadR :: BotanPadRef -> (BotanPad -> IO (BotanPad,a) ) -> IO a
withBotanPadR siocb cb = deRefStablePtr siocb >>= (\ bpmvar -> modifyMVar bpmvar cb )

foreign export ccall iocba_push :: BotanPadRef -> Ptr CChar -> Int -> IO ()
iocba_push siocb p len = do
    push_action <- withBotanPadR siocb $ \ botan_pad ->  do
        let
            push_action = botan_pad ^.  (encryptedSide_BP . pushAction_IOC )
        return (botan_pad, push_action)
    let cstr = (p, fromIntegral len)
    b <- B.packCStringLen cstr
    push_action (LB.fromStrict b)


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
iocba_handshake_cb siocb = do
  withBotanPad siocb $ \botan_pad -> do
      tryPutMVar (botan_pad ^. handshakeCompleted_BP) ()
      return $ set sessionState_BP Functioning_BSS botan_pad


foreign export ccall iocba_select_protocol_cb :: BotanPadRef -> Ptr CChar -> Int -> IO Int
iocba_select_protocol_cb siocb p len = withBotanPadR siocb $ \ botan_pad -> do
    let
        cstr = (p, fromIntegral len)
        selector = botan_pad ^. protocolSelector_BP
    b <- B.packCStringLen cstr
    cp@(chosen_protocol_int, _chosen_protocol_str) <- selector b
    let

        new_botan_pad = set selectedProtocol_BP cp botan_pad
    return (new_botan_pad, chosen_protocol_int)


foreign import ccall iocba_cleartext_push :: BotanTLSChannelPtr -> Ptr CChar -> Int -> IO ()

foreign import ccall "&iocba_delete_tls_context" iocba_delete_tls_context :: FunPtr( BotanTLSContextCSidePtr -> IO () )

foreign import ccall iocba_make_tls_context :: CString -> CString -> IO BotanTLSContextCSidePtr

foreign import ccall "&iocba_delete_tls_server_channel" iocba_delete_tls_server_channel :: FunPtr( BotanTLSChannelPtr -> IO () )

--foreign import ccall "wrapper" mkTlsServerDeleter :: (BotanTLSChannelPtr -> IO ()) -> IO (FunPtr ( BotanTLSChannelPtr -> IO () ) )

foreign import ccall iocba_receive_data :: BotanTLSChannelPtr -> Ptr CChar -> Int -> IO ()


botanPushData :: MVar BotanPad -> LB.ByteString -> IO ()
botanPushData botan_pad_mvar datum = do
    let
        strict_datum = LB.toStrict datum
    (maybe_problem, channel) <- withMVar botan_pad_mvar $ \botan_pad -> do
        let
            channel = botan_pad ^. tlsChannel_BP
            problem_mvar = botan_pad ^. problem_BP
        mp <- tryReadMVar problem_mvar
        return (mp, channel)
    case maybe_problem of
        Nothing ->
            Un.unsafeUseAsCStringLen strict_datum $ \ (pch, len) -> do
              withForeignPtr channel $ \ c ->
                 iocba_cleartext_push c pch len

        Just problem ->
            E.throwIO problem


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
                    had_problem_mvar = botan_pad ^. problem_BP
                maybe_had_problem <- tryReadMVar had_problem_mvar
                case maybe_had_problem of
                    Nothing ->
                        return (new_botan_pad, datum)
                    Just problem ->
                        E.throwIO problem
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
               had_problem_mvar = botan_pad ^. problem_BP
               len_datum :: Int64
               len_datum = fromIntegral . LB.length $ datum
               (set_back_bu, use, new_n)   |   len_datum <= n       =  (mempty, available_bu, n - len_datum)
                                           |   otherwise            =  ( Bu.lazyByteString . LB.drop (len_datum - n) $ datum
                                                                       , Bu.lazyByteString . LB.take n $ datum
                                                                       , 0 )
               new_botan_pad = set availableData_BP set_back_bu botan_pad
               new_gotten = gotten `mappend` use
            maybe_had_problem <- tryReadMVar had_problem_mvar
            case maybe_had_problem of
                Nothing ->
                    return (new_botan_pad, (new_gotten, new_n))
                Just problem ->
                    E.throwIO problem
        if nn == 0
            then return ng
        else
            consume data_came_mvar nn ng


foreign import ccall iocba_new_tls_server_channel ::
     BotanPadRef
     -> BotanTLSContextCSidePtr
     -> Int          -- protocol selection policty
     -> IO BotanTLSChannelPtr


protocolSelectorToC :: ProtocolSelector -> B.ByteString -> IO (Int, B.ByteString)
protocolSelectorToC prot_sel flat_protocols = do
    let
        protocol_list =  B.split 0 flat_protocols
    selected <- prot_sel protocol_list
    let
        maybe_idx = elemIndex selected protocol_list
    return (fromMaybe 0 maybe_idx, selected)

newBotanTLSContext :: RawFilePath -> RawFilePath -> ProtocolSelector ->  IO BotanTLSContext
newBotanTLSContext cert_filename privkey_filename prot_sel = do
    let

    B.useAsCString cert_filename $ \ s1 ->
        B.useAsCString privkey_filename $ \ s2 -> do
            ctx_ptr <-  iocba_make_tls_context s1 s2
            x <- newForeignPtr iocba_delete_tls_context ctx_ptr
            return $ BotanTLSContext x prot_sel


unencryptChannelData :: TLSServerIO a =>  BotanTLSContext -> a -> IO BotanSession
unencryptChannelData botan_ctx tls_data  = do
    let
        fctx = botan_ctx ^. cppSide_BTC
        protocol_selector = botan_ctx ^. protocolSelector_BTC
    botan_pad_mvar <- newEmptyMVar
    botan_pad_stable_ref <- newStablePtr botan_pad_mvar


    tls_channel_ptr <- withForeignPtr fctx $ \ x -> iocba_new_tls_server_channel botan_pad_stable_ref x 1
    tls_channel_fptr <- newForeignPtr iocba_delete_tls_server_channel tls_channel_ptr
    tls_io_callbacks <- handshake tls_data
    data_came_mvar <- newEmptyMVar
    handshake_completed_mvar <- newEmptyMVar
    problem_mvar <- newEmptyMVar


    let
        new_botan_pad = BotanPad {
            _encryptedSide_BP      = tls_io_callbacks
          , _availableData_BP      = mempty
          , _sessionState_BP       = WaitingHandshake_BSS
          , _tlsChannel_BP         = tls_channel_fptr
          , _dataCame_BP           = data_came_mvar
          , _handshakeCompleted_BP = handshake_completed_mvar
          , _protocolSelector_BP   = protocolSelectorToC protocol_selector
          , _selectedProtocol_BP   = (0, mempty) -- Filler
          , _problem_BP            = problem_mvar
          }

        tls_pull_data_action = tls_io_callbacks ^. bestEffortPullAction_IOC

        pump_exc_handler :: IOProblem -> IO (Maybe B.ByteString)
        pump_exc_handler p = putMVar problem_mvar p >> return Nothing

        pump :: IO ()
        pump = do
            maybe_new_data <- E.catch
                (Just <$> tls_pull_data_action True)
                pump_exc_handler
            case maybe_new_data of
                Just new_data -> do
                    Un.unsafeUseAsCStringLen new_data $ \ (pch, len) ->
                        iocba_receive_data tls_channel_ptr pch len
                    pump
                Nothing ->
                    -- On exceptions, finish this thread
                    return ()

    -- Create the pump thread
    forkIO pump

    putMVar botan_pad_mvar new_botan_pad
    mkWeakMVar botan_pad_mvar $ do
        freeStablePtr botan_pad_stable_ref

    return $ BotanSession botan_pad_mvar


instance IOChannels BotanSession where
    handshake _botan_session@(BotanSession botan_pad_mvar) = do
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

        handshake_completed_mvar <- withMVar botan_pad_mvar ( \ botan_pad -> return (botan_pad ^. handshakeCompleted_BP ))

        -- We will be waiting for the handshake...
        readMVar handshake_completed_mvar


        return $ IOCallbacks {
            _pushAction_IOC = push_action
          , _pullAction_IOC = pull_action
          , _bestEffortPullAction_IOC = best_effort_pull_action
          , _closeAction_IOC = close_action
            }
