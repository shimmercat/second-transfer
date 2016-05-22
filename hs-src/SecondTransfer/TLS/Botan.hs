{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell, Rank2Types, FunctionalDependencies, OverloadedStrings #-}
module SecondTransfer.TLS.Botan (
                  BotanTLSContext
                , BotanSession
                , unencryptChannelData
                , newBotanTLSContext
                , newBotanTLSContextFromMemory
                , botanTLS
       ) where

import           Control.Concurrent
import qualified Control.Exception                                         as E
import           Control.Monad                                             (unless, when)
import           Foreign
import           Foreign.C.Types                                           (CChar, CInt(..), CUInt(..))
import           Foreign.C.String                                          (CString)

--import           Data.List                                                 (elemIndex)
-- import           Data.Tuple                                                (swap)
import           Data.Typeable                                             (Proxy(..))
import           Data.Maybe                                                (fromMaybe)
import           Data.IORef
import qualified Data.ByteString                                           as B
import qualified Data.ByteString.Builder                                   as Bu
import qualified Data.ByteString.Lazy                                      as LB
import qualified Data.ByteString.Unsafe                                    as Un

import           Control.Lens                                              ( (^.), makeLenses,
                                                                             --set, Lens'
                                                                           )

import           System.IO.Unsafe                                          (unsafePerformIO)

-- Import all of it!
import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.MainLoop.Protocol
import           SecondTransfer.Exception                                  (
                                                                             IOProblem
                                                                           , NoMoreDataException(..)
                                                                           , TLSEncodingIssue (..)
                                                                           , keyedReportExceptions
                                                                           , forkIOExc
                                                                           )
import           SecondTransfer.TLS.Types                                  (  TLSContext (..) )

#include "instruments.cpphs"

data BotanTLSChannel

type RawFilePath = B.ByteString

type BotanTLSChannelPtr = Ptr BotanTLSChannel

type BotanTLSChannelFPtr = ForeignPtr BotanTLSChannel

data BotanPad = BotanPad {
    _encryptedSide_BP      :: IOCallbacks
  , _availableData_BP      :: IORef Bu.Builder
  , _tlsChannel_BP         :: IORef BotanTLSChannelFPtr
  , _dataCame_BP           :: MVar ()
  , _handshakeCompleted_BP :: MVar ()
  , _writeLock_BP          :: MVar ()
  , _active_BP             :: MVar ()
  , _selectedProtocol_BP   :: MVar HttpProtocolVersion
  , _problem_BP            :: MVar ()
    }

-- type BotanPadRef = StablePtr BotanPad

makeLenses ''BotanPad

newtype BotanSession = BotanSession (IORef BotanPad)

data BotanTLSContextAbstract

type BotanTLSContextCSidePtr = Ptr BotanTLSContextAbstract

type BotanTLSContextCSideFPtr = ForeignPtr BotanTLSContextAbstract

data BotanTLSContext = BotanTLSContext {
    _cppSide_BTC          :: BotanTLSContextCSideFPtr
  --, _protocolSelector_BTC :: ProtocolSelector
    }

makeLenses ''BotanTLSContext


dontMultiThreadBotan :: MVar ()
{-# NOINLINE dontMultiThreadBotan #-}
dontMultiThreadBotan = unsafePerformIO $ do
    newMVar ()

foreign import ccall iocba_cleartext_push ::
  BotanTLSChannelPtr
  -> Ptr CChar
  -> Word32
  -> Ptr CChar
  -> Ptr Word32
  -> IO Int32

foreign import ccall iocba_close ::
    BotanTLSChannelPtr ->
    Ptr CChar ->
    Ptr Word32 ->
    IO ()

foreign import ccall "&iocba_delete_tls_context" iocba_delete_tls_context :: FunPtr( BotanTLSContextCSidePtr -> IO () )

foreign import ccall iocba_make_tls_context :: CString -> CString -> Int32 -> IO BotanTLSContextCSidePtr

foreign import ccall iocba_make_tls_context_from_memory :: CString -> CUInt -> CString -> CUInt -> Int32 -> IO BotanTLSContextCSidePtr

foreign import ccall "&iocba_delete_tls_server_channel" iocba_delete_tls_server_channel :: FunPtr( BotanTLSChannelPtr -> IO () )

foreign import ccall unsafe "iocba_maybe_get_protocol" iocba_maybe_get_protocol ::
    BotanTLSChannelPtr ->
    IO Int32

foreign import ccall unsafe "iocba_handshake_completed" iocba_handshake_completed ::
    BotanTLSChannelPtr ->
    IO Int32

foreign import ccall unsafe "iocba_peer_closed_transport" iocba_peer_closed_transport::
    BotanTLSChannelPtr ->
    IO Int32

foreign import ccall iocba_receive_data ::
    BotanTLSChannelPtr ->
    Ptr CChar ->
    Word32 ->
    Ptr CChar ->
    Ptr Word32 ->
    Ptr CChar ->
    Ptr Word32 ->
    IO Int32

foreign import ccall unsafe "iocba_alert_produced" iocba_alert_produced ::
    BotanTLSChannelPtr ->
    IO Int32


botanPushData :: BotanPad -> LB.ByteString -> IO ()
botanPushData botan_pad datum = do
    let
        strict_datum = LB.toStrict datum
        reserve_length :: Int
        reserve_length =
            floor ((fromIntegral $ B.length strict_datum) * 1.2 + 4096.0 :: Double)
        write_lock = botan_pad ^. writeLock_BP
        channel_ioref = botan_pad ^. tlsChannel_BP
        problem_mvar = botan_pad ^. problem_BP
        handshake_completed_mvar = botan_pad ^. handshakeCompleted_BP

    -- We are doing a lot of locking here... is it all needed?
    readMVar handshake_completed_mvar
    channel <- readIORef channel_ioref
    data_to_send <- withMVar write_lock $ \ _ -> do
        mp <- tryReadMVar problem_mvar
        case mp of
            Nothing ->
                allocaBytes reserve_length $ \ out_enc_to_send -> do
                  alloca $ \ p_enc_to_send_length -> do
                    poke p_enc_to_send_length (fromIntegral reserve_length)
                    Un.unsafeUseAsCStringLen strict_datum $ \ (cleartext_pch, cleartext_len) ->
                      withForeignPtr channel $ \ c -> withMVar dontMultiThreadBotan . const $ do
                         push_result <- iocba_cleartext_push
                             c
                             cleartext_pch
                             (fromIntegral cleartext_len)
                             out_enc_to_send
                             p_enc_to_send_length
                         if push_result < 0
                            then E.throwIO TLSEncodingIssue
                            else do
                                enc_to_send_length <- peek p_enc_to_send_length
                                B.packCStringLen (out_enc_to_send, fromIntegral enc_to_send_length)


            Just _ -> do
                --(botan_pad ^. encryptedSide_BP . closeAction_IOC )
                --putStrLn "bpRaise"
                E.throwIO $ NoMoreDataException

    (botan_pad ^. encryptedSide_BP . pushAction_IOC) . LB.fromStrict $ data_to_send


-- Bad things will happen if serveral threads are calling this concurrently!!
pullAvailableData :: BotanPad -> Bool -> IO LB.ByteString
pullAvailableData botan_pad can_wait = do
    let
        data_came_mvar = botan_pad ^. dataCame_BP
        avail_data_iorref = botan_pad ^. availableData_BP
        io_problem_mvar = botan_pad ^. problem_BP
        -- And here inside cleanup. The block below doesn't compose with other instances of
        -- reads happening simultaneously
    avail_data <- atomicModifyIORef' avail_data_iorref $ \ bu -> (mempty, bu)
    let
        avail_data_lb = Bu.toLazyByteString avail_data
        avail_data_length_w64 =  LB.length avail_data_lb
    case ( avail_data_length_w64 == 0 , can_wait ) of
        (True, True) -> do
            -- Just wait for more data coming here
            takeMVar data_came_mvar
            maybe_problem <- tryReadMVar io_problem_mvar
            case maybe_problem of
                Nothing -> do
                    pullAvailableData botan_pad True

                Just () -> do
                    -- Allow others to wake-up
                    putMVar data_came_mvar ()
                    E.throwIO NoMoreDataException

        (_, _) -> do
            let
                result =  avail_data_lb
            return result


foreign import ccall iocba_new_tls_server_channel ::
        BotanTLSContextCSidePtr
     -> IO BotanTLSChannelPtr


newBotanTLSContext :: RawFilePath -> RawFilePath -> HttpProtocolVersion ->  IO BotanTLSContext
newBotanTLSContext cert_filename privkey_filename prefer_protocol =
    let
      prot_sel = case prefer_protocol of
          Http11_HPV -> 1
          Http2_HPV -> 2
    in withMVar dontMultiThreadBotan $ \ _ ->  do
        B.useAsCString cert_filename $ \ s1 ->
            B.useAsCString privkey_filename $ \ s2 -> do
                ctx_ptr <-  iocba_make_tls_context s1 s2 prot_sel
                x <- newForeignPtr iocba_delete_tls_context ctx_ptr
                return $ BotanTLSContext x


newBotanTLSContextFromMemory :: B.ByteString -> B.ByteString -> HttpProtocolVersion -> IO BotanTLSContext
newBotanTLSContextFromMemory cert_data key_data prefer_protocol =
    let
      prot_sel = case prefer_protocol of
          Http11_HPV -> 1
          Http2_HPV -> 2
    in  withMVar dontMultiThreadBotan $ \ _ ->   do
        B.useAsCString cert_data $ \ s1 ->
          B.useAsCString key_data $ \s2 -> do
              ctx_ptr <- iocba_make_tls_context_from_memory
                             s1
                             (fromIntegral . B.length $ cert_data)
                             s2
                             (fromIntegral . B.length $ key_data)
                             prot_sel
              x <- newForeignPtr iocba_delete_tls_context ctx_ptr
              return $ BotanTLSContext x


unencryptChannelData :: TLSServerIO a =>  BotanTLSContext -> a -> IO  BotanSession
unencryptChannelData botan_ctx tls_data  = do
    let
        fctx = botan_ctx ^. cppSide_BTC

    tls_io_callbacks <- handshake tls_data
    data_came_mvar <- newEmptyMVar
    handshake_completed_mvar <- newEmptyMVar
    problem_mvar <- newEmptyMVar
    selected_protocol_mvar <- newEmptyMVar
    active_mvar <- newMVar ()
    tls_channel_ioref <- newIORef (error "")
    avail_data_ioref <- newIORef ""
    write_lock_mvar <- newMVar ()

    let
        new_botan_pad = BotanPad {
            _encryptedSide_BP      = tls_io_callbacks
          , _availableData_BP      = avail_data_ioref
          , _tlsChannel_BP         = tls_channel_ioref
          , _dataCame_BP           = data_came_mvar
          , _handshakeCompleted_BP = handshake_completed_mvar
          , _writeLock_BP          = write_lock_mvar
          -- , _protocolSelector_BP   = protocolSelectorToC protocol_selector
          , _selectedProtocol_BP   = selected_protocol_mvar
          , _problem_BP            = problem_mvar
          , _active_BP             = active_mvar
          }

        tls_pull_data_action = tls_io_callbacks ^. bestEffortPullAction_IOC

        destructor =
            -- withMVar dontMultiThreadBotan . const $
               iocba_delete_tls_server_channel

    -- botan_pad_stable_ref <- newStablePtr new_botan_pad

    tls_channel_ptr <- withForeignPtr fctx $ \ x -> iocba_new_tls_server_channel  x

    tls_channel_fptr <- newForeignPtr destructor tls_channel_ptr

    let

        pump_exc_handler :: IOProblem -> IO (Maybe LB.ByteString)
        pump_exc_handler _ = do
            _ <- tryPutMVar problem_mvar ()
            -- Wake-up any readers...
            _ <- tryPutMVar data_came_mvar ()
            return Nothing

        -- Reserve extra
        reserve_extra x | x < 4  = 32000
                        | otherwise = 2048

        -- Feeds encrypted data to Botan
        pump :: Int -> IO ()
        pump serie = do
            maybe_new_data <- E.catch
                (Just <$> tls_pull_data_action True)
                pump_exc_handler
            case maybe_new_data of
                Just new_data
                  | len_new_data <- LB.length new_data, len_new_data > 0 -> do
                    can_continue <- withMVar write_lock_mvar $ \_ -> do
                        maybe_problem <- tryReadMVar problem_mvar
                        case maybe_problem of
                            Nothing -> do
                                maybe_cont_data <- do
                                  let

                                    cleartext_reserve_length :: Int
                                    cleartext_reserve_length = floor $
                                      ((fromIntegral len_new_data * 1.2) :: Double) +
                                      2048.0

                                    enc_reserve_length :: Int
                                    enc_reserve_length = floor $
                                      ((fromIntegral len_new_data * 1.2) :: Double) +
                                      fromIntegral (reserve_extra serie)

                                  allocaBytes cleartext_reserve_length $ \ p_clr_space ->
                                    allocaBytes enc_reserve_length $ \ p_enc_to_send ->
                                       alloca $ \ p_enc_to_send_length ->
                                         alloca $ \ p_clr_length -> do
                                                poke p_enc_to_send_length $
                                                    fromIntegral enc_reserve_length
                                                poke p_clr_length $
                                                    fromIntegral cleartext_reserve_length
                                                Un.unsafeUseAsCStringLen (LB.toStrict new_data) $ \ (enc_pch, enc_len) ->
                                                    withMVar dontMultiThreadBotan . const $ do
                                                        engine_result <- iocba_receive_data
                                                            tls_channel_ptr
                                                            enc_pch
                                                            (fromIntegral enc_len)
                                                            p_enc_to_send
                                                            p_enc_to_send_length
                                                            p_clr_space
                                                            p_clr_length

                                                        if engine_result < 0
                                                          then do
                                                            -- putStrLn "BadEngineResult"
                                                            _ <- tryPutMVar problem_mvar ()
                                                            -- Just awake any variables
                                                            _ <- tryPutMVar data_came_mvar ()
                                                            return Nothing
                                                          else do
                                                            enc_to_send_length <- peek p_enc_to_send_length
                                                            clr_length <- peek p_clr_length
                                                            to_send <- B.packCStringLen (p_enc_to_send, fromIntegral enc_to_send_length)
                                                            to_return <- B.packCStringLen (p_clr_space, fromIntegral clr_length)
                                                            -- Do I have a encoding?
                                                            have_protocol <- iocba_maybe_get_protocol tls_channel_ptr
                                                            case have_protocol of
                                                                0 -> return ()
                                                                1 -> tryPutMVar selected_protocol_mvar Http11_HPV >> return ()
                                                                2 -> tryPutMVar selected_protocol_mvar Http2_HPV >> return ()
                                                                _ -> return ()
                                                            handshake_completed <- iocba_handshake_completed tls_channel_ptr
                                                            when (handshake_completed /= 0) $
                                                                (tryPutMVar handshake_completed_mvar  ()) >> return ()
                                                            peer_closed_transport <- iocba_peer_closed_transport tls_channel_ptr
                                                            -- Do provisions for tranport closed after this call
                                                            when (peer_closed_transport /= 0) $
                                                                tryPutMVar problem_mvar () >> return ()
                                                            return $ Just  (to_send, to_return)

                                case maybe_cont_data of
                                    Just (send_to_peer, just_unencrypted) -> do
                                        unless (B.length send_to_peer == 0) $ do
                                            either_problem <- E.try $
                                                (tls_io_callbacks ^. pushAction_IOC) .
                                                    LB.fromStrict $
                                                    send_to_peer
                                            case either_problem :: Either IOProblem () of
                                                Left _ -> do
                                                    _ <- tryPutMVar problem_mvar ()
                                                    _ <- tryPutMVar data_came_mvar ()
                                                    return ()
                                                Right _ -> return ()
                                        unless (B.length just_unencrypted == 0 )  $ do
                                            atomicModifyIORef' avail_data_ioref $ \ bu ->
                                              (
                                                bu `seq` (bu `mappend` Bu.byteString just_unencrypted),
                                                ()
                                              )
                                            -- putStrLn "cleartext data!!"
                                            _ <- tryPutMVar data_came_mvar ()
                                            return ()
                                        return True

                                    Nothing -> return False
                            Just _ -> do
                                return False
                    if can_continue
                      then do
                        pump (serie + 1)
                      else
                        return ()
                  | otherwise -> do
                    -- This is actually an error
                    pump (serie + 1)

                Nothing -> do
                    -- On exceptions, finish this thread
                    return ()

    writeIORef tls_channel_ioref tls_channel_fptr

    result <- newIORef new_botan_pad

    _ <- mkWeakIORef result $ do
        closeBotan new_botan_pad
        -- freeStablePtr botan_pad_stable_ref

    -- Create the pump thread
    _ <- forkIOExc "BotanPump" (pump 0)

    return $ BotanSession result


closeBotan :: BotanPad -> IO ()
closeBotan botan_pad =
  do
    tls_channel_fptr <- readIORef (botan_pad ^. tlsChannel_BP)
    withForeignPtr tls_channel_fptr $ \ tls_channel_ptr -> do
        maybe_gotit <- tryTakeMVar (botan_pad ^. active_BP)
        case maybe_gotit of
            Just _ -> do
                -- Let's forbid libbotan for sending more output
                _ <- tryPutMVar (botan_pad ^. problem_BP) ()
                -- Cleanly close tls, if nobody else is writing there
                last_msg <- allocaBytes 1024  $ \ p_enc_to_send -> do
                  alloca $ \ p_enc_to_send_length ->
                            withMVar (botan_pad ^. writeLock_BP) $ \ _ -> do
                                poke p_enc_to_send_length 1023
                                withMVar dontMultiThreadBotan . const $
                                    iocba_close
                                        tls_channel_ptr
                                        p_enc_to_send
                                        p_enc_to_send_length
                                bytes_avail <- peek p_enc_to_send_length
                                last_message <- B.packCStringLen (p_enc_to_send, fromIntegral bytes_avail)
                                _ <- tryPutMVar (botan_pad ^. problem_BP) ()
                                _ <- tryPutMVar (botan_pad ^. dataCame_BP) ()
                                return last_message

                -- Send the last message, if possible at all... otherwise just ignore the
                -- exception
                either_sent <- E.try $ do
                    (botan_pad ^. encryptedSide_BP . pushAction_IOC) (LB.fromStrict last_msg)

                -- Close the cypher-text transport
                either_sent2 <- E.try $ (botan_pad ^. encryptedSide_BP . closeAction_IOC)

                -- Ignore any exceptions that happen when closing the transport, they are all
                -- too usual when the peer closes first and such...
                case either_sent :: Either IOProblem () of
                    Left  _ -> return ()
                    Right _ -> return ()

                case either_sent2 :: Either IOProblem () of
                    Left  _ -> return ()
                    Right _ -> return ()


            Nothing ->
                -- Thing already closed, don't do it again
                return ()


instance IOChannels BotanSession where

    handshake _botan_session@(BotanSession botan_ioref) = do

        let
            -- Capture the IORef to avoid early finalization
            push_action :: PushAction
            push_action bs = do
                botan_pad <- readIORef botan_ioref
                botanPushData botan_pad bs

            best_effort_pull_action :: BestEffortPullAction
            best_effort_pull_action x = do
                botan_pad <- readIORef botan_ioref
                pullAvailableData botan_pad x

        pull_action_wrapping <- newPullActionWrapping best_effort_pull_action

        botan_pad' <- readIORef botan_ioref
        let
            pull_action :: PullAction
            pull_action = pullFromWrapping' pull_action_wrapping

            best_effort_pull_action' = bestEffortPullFromWrapping pull_action_wrapping

            close_action :: CloseAction
            close_action = closeBotan botan_pad'

            -- handshake_completed_mvar = botan_pad' ^. handshakeCompleted_BP

        -- We needed to wait for a hand-shake
        -- readMVar handshake_completed_mvar

        return $ IOCallbacks {
            _pushAction_IOC = push_action
          , _pullAction_IOC = pull_action
          , _bestEffortPullAction_IOC = best_effort_pull_action'
          , _closeAction_IOC = close_action
            }


instance TLSContext BotanTLSContext BotanSession where
    newTLSContextFromMemory = newBotanTLSContextFromMemory
    newTLSContextFromCertFileNames = newBotanTLSContext
    unencryptTLSServerIO = unencryptChannelData
    getSelectedProtocol (BotanSession pad_ioref) = do
        botan_pad <- readIORef pad_ioref
        -- putStrLn "Asked for protocol"
        a <- readMVar (botan_pad ^. selectedProtocol_BP)
        -- putStrLn $ "got protocol: " ++ show a
        return a


botanTLS :: Proxy BotanTLSContext
botanTLS = Proxy
