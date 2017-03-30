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
--import           Data.Maybe                                                (fromMaybe)
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
                                                                             IOProblem(..)
                                                                           , NoMoreDataException(..)
                                                                           , TLSBufferIsTooSmall(..)
                                                                           , TLSEncodingIssue (..)
                                                                           --, keyedReportExceptions
                                                                           , forkIOExc
                                                                           , DerivedDataException (..)
                                                                           )
import           SecondTransfer.TLS.Types                                  (  TLSContext (..) )
import           SecondTransfer.TLS.SessionStorage

--import           Debug.Trace

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
  -- | active_BP : Protects Botan of multiple close attempts
  , _active_BP             :: MVar ()
  , _selectedProtocol_BP   :: MVar HttpProtocolVersion
  , _problem_BP            :: MVar IOProblem
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

foreign import ccall "iocba_delete_tls_server_channel" iocba_delete_tls_server_channel' ::  BotanTLSChannelPtr -> IO ()

-- extern "C" DLL_PUBLIC void iocba_enable_sessions(
--     botan_tls_context_t* ctx,
--     save_fptr save_p,
--     remove_entry_fptr remove_entry_p,
--     load_fptr load_p,
--     session_lifetime_fptr session_lifetime_p,
--     encryption_key_fptr encryption_key_p
--     )

foreign import ccall "iocba_enable_sessions" iocba_enable_sessions ::
    BotanTLSContextCSidePtr ->
    FunPtr Save_Pre ->
    FunPtr RemoveEntry_Pre ->
    FunPtr Load_Pre ->
    FunPtr SessionLifetime_Pre ->
    FunPtr EncryptionKey_Pre ->
    IO ()

foreign import ccall {- unsafe -} "iocba_maybe_get_protocol" iocba_maybe_get_protocol ::
    BotanTLSChannelPtr ->
    IO Int32

foreign import ccall unsafe "iocba_handshake_completed" iocba_handshake_completed ::
    BotanTLSChannelPtr ->
    IO Int32

foreign import ccall unsafe "iocba_session_was_resumed" iocba_session_was_resumed ::
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

foreign import ccall unsafe "iocba_alert_is_fatal" iocba_alert_is_fatal ::
    BotanTLSChannelPtr ->
    IO Int32


-- | Called with cleartext data we want to encode and send to the peer.
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

    -- Wait for handshake_completed_mvar . This variable is set on successfull handshake,
    -- but also on a failed one....
    readMVar handshake_completed_mvar
    channel <- readIORef channel_ioref
    data_to_send <- withMVar write_lock $ \ _ -> do
        -- Therefore, the first thing we do when we wake up is to check if there has been
        -- any problems with the engine. If none, proceed normally. If there has been
        -- a problem, raise an exception to the call chain, which eventually involves
        -- the client of this module.
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


-- | Implements the pullAction operation of the IOCallbacks.
-- In simple terms, it reads unencrypted data from the Botan engine and returns
-- it to the client layer on demand.
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

                Just e -> do
                    -- Allow others to wake-up
                    putMVar data_came_mvar ()
                    E.throwIO .  DerivedDataException $ e

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


-- | Creates a flow of encrypted data from the peer to Botan
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
          -- Callbacks with encrypted data
            _encryptedSide_BP      = tls_io_callbacks
          -- Pointer to the C-side Botan context.
          , _tlsChannel_BP         = tls_channel_ioref
          -- The two variables below are notification vars for data flowing
          -- in each of the two directions
          , _availableData_BP      = avail_data_ioref
          , _dataCame_BP           = data_came_mvar
          -- Signals that the handshake has completed
          , _handshakeCompleted_BP = handshake_completed_mvar
          -- Exclusive lock for writing and interacting with Botan
          , _writeLock_BP          = write_lock_mvar
          , _selectedProtocol_BP   = selected_protocol_mvar
          --  Signal to read-write pumps that there has been a problem, and that
          --  no normal client data should be transported.
          , _problem_BP            = problem_mvar
          -- Full by default, when the connection is active, otherwise it
          -- is empty (closeBotan empties this variable.)
          , _active_BP             = active_mvar
          }
        --tls_pull_data_action = tls_io_callbacks ^. bestEffortPullAction_IOC

    tls_channel_ptr <- withForeignPtr fctx $ \ x -> iocba_new_tls_server_channel  x
    -- tls_channel_fptr <- newForeignPtr iocba_delete_server_channel
    tls_channel_fptr <- newForeignPtr_ tls_channel_ptr
    writeIORef tls_channel_ioref tls_channel_fptr
    result <- newIORef new_botan_pad
    _ <- mkWeakIORef result $ do
        closeBotan new_botan_pad
        -- freeStablePtr botan_pad_stable_ref
    -- Create the pump thread
    _ <- forkIOExc "BotanPump" (encryptedToBotan new_botan_pad)
    return $ BotanSession result


encryptedToBotan :: BotanPad -> IO ()
encryptedToBotan botan_pad   =
    pump 0
  where
    -- Let's burst the pad
    problem_mvar = botan_pad ^. problem_BP
    data_came_mvar = botan_pad ^. dataCame_BP
    handshake_completed_mvar = botan_pad ^. handshakeCompleted_BP
    tls_channel_ioref = botan_pad ^. tlsChannel_BP
    tls_io_callbacks = botan_pad ^. encryptedSide_BP
    tls_pull_data_action = tls_io_callbacks ^. bestEffortPullAction_IOC
    selected_protocol_mvar = botan_pad ^. selectedProtocol_BP
    write_lock_mvar = botan_pad ^. writeLock_BP
    avail_data_ioref = botan_pad ^. availableData_BP

    -- Reserve extra: we need a big input buffer size for POST
    -- with multipart/form-data and several files, those can upload
    -- pretty big TLS segments.
    reserve_extra x | x < 4  = 32000
                    | otherwise = 32000

    pump_exc_handler :: IOProblem -> IO (Maybe LB.ByteString)
    pump_exc_handler exc = do
        _ <- tryPutMVar problem_mvar $ IOProblem (DerivedDataException exc)
        -- Wake-up any readers...
        _ <- tryPutMVar data_came_mvar ()
        return Nothing

    -- Create return buffers with enough length to satisfy
    -- the TLS handshake
    reserve_buffer_lengths :: Int -> Int -> (Int, Int)
    reserve_buffer_lengths len_arrived_data  serie =
      let
        m = fromIntegral len_arrived_data

        cleartext_reserve_length :: Int
        cleartext_reserve_length = floor $
          ((m * 1.2) :: Double) +
          fromIntegral (reserve_extra serie :: Int)

        enc_reserve_length :: Int
        enc_reserve_length = floor $
          ((m * 1.2) :: Double) +
          fromIntegral (reserve_extra serie :: Int)
      in (cleartext_reserve_length, enc_reserve_length)

    do_message_exchange
        p_clr_space
        p_enc_to_send
        p_enc_to_send_length
        p_clr_length
        tls_channel_ptr
        enc_reserve_length
        cleartext_reserve_length
        new_data
      = do
        poke p_enc_to_send_length $
            fromIntegral enc_reserve_length
        poke p_clr_length $
            fromIntegral cleartext_reserve_length
        -- So we are taking the just received data and making it
        -- available through a pointer.
        Un.unsafeUseAsCStringLen (LB.toStrict new_data) $ \ (enc_pch, enc_len) -> do
            -- No concurrent calls to Botan, the damn library don't like those
            engine_result  <- withMVar dontMultiThreadBotan . const $ do
                iocba_receive_data
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
                _ <- tryPutMVar problem_mvar (IOProblem TLSBufferIsTooSmall)
                -- Just awake any variables
                _ <- tryPutMVar data_came_mvar ()
                _ <- tryPutMVar handshake_completed_mvar ()
                _ <- tryPutMVar
                    selected_protocol_mvar
                    (E.throw $ TLSEncodingIssue)
                return Nothing
              else do
                enc_to_send_length <- peek p_enc_to_send_length
                clr_length <- peek p_clr_length
                to_send <- B.packCStringLen (p_enc_to_send, fromIntegral enc_to_send_length)
                to_return <- B.packCStringLen (p_clr_space, fromIntegral clr_length)
                -- Do I have a encoding?
                have_protocol <- iocba_maybe_get_protocol
                    tls_channel_ptr
                handshake_completed <- iocba_handshake_completed tls_channel_ptr
                case have_protocol of
                    0 -> if handshake_completed /= 0
                           then error "ProtocolMustHasBeenChosenByNow"
                           else tryPutMVar selected_protocol_mvar Http11_HPV >> return ()
                    1 -> tryPutMVar selected_protocol_mvar Http11_HPV >> return ()
                    2 -> tryPutMVar selected_protocol_mvar Http2_HPV >> return ()
                    -- The code below is just a stupid default, if another protocol was indicated.
                    _ -> tryPutMVar selected_protocol_mvar Http11_HPV >> return ()
                when (handshake_completed /= 0) $
                    (tryPutMVar handshake_completed_mvar  ()) >> return ()
                peer_closed_transport <- iocba_peer_closed_transport tls_channel_ptr
                -- Do provisions for tranport closed after this call
                when (peer_closed_transport /= 0) $
                    tryPutMVar problem_mvar (IOProblem NoMoreDataException) >> return ()
                return $ Just  (to_send, to_return)


    stop_this_channel exc = do
         _ <- tryPutMVar problem_mvar exc
         _ <- tryPutMVar data_came_mvar ()
         return ()

    send_and_read_data :: Int -> LB.ByteString -> Int -> IO Bool
    send_and_read_data serie new_data len_new_data = do
        can_continue <- withMVar write_lock_mvar  $ \_ -> do
            maybe_problem <- tryReadMVar problem_mvar
            case maybe_problem of
                Nothing -> do
                    -- OK, we can go, no problems detected
                    maybe_cont_data <- do
                      let
                          (cleartext_reserve_length, enc_reserve_length) =
                              reserve_buffer_lengths len_new_data serie

                      allocaBytes cleartext_reserve_length $ \ p_clr_space ->
                        allocaBytes enc_reserve_length $ \ p_enc_to_send ->
                           alloca $ \ p_enc_to_send_length ->
                             alloca $ \ p_clr_length -> do
                                 tls_channel_fptr <- readIORef tls_channel_ioref
                                 withForeignPtr tls_channel_fptr $ \ tls_channel_ptr ->
                                     do_message_exchange
                                         p_clr_space
                                         p_enc_to_send
                                         p_enc_to_send_length
                                         p_clr_length
                                         tls_channel_ptr
                                         enc_reserve_length
                                         cleartext_reserve_length
                                         new_data


                    case maybe_cont_data of
                        Just (send_to_peer, just_unencrypted) -> do
                            unless (B.length send_to_peer == 0) $ do
                                either_problem <- E.try $
                                    (tls_io_callbacks ^. pushAction_IOC) .
                                        LB.fromStrict $
                                        send_to_peer
                                case either_problem :: Either IOProblem () of
                                    Left e -> stop_this_channel e
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

        return can_continue

    pump :: Int -> IO ()
    pump serie = do
        maybe_new_data <- E.catch
            (Just <$> tls_pull_data_action True)
            pump_exc_handler
        case maybe_new_data of
            Just new_data
              -- new_data is encrypted data we want to process
              | len_new_data <- LB.length new_data, len_new_data > 0 -> do
                    can_continue <- send_and_read_data
                        serie
                        new_data
                        (fromIntegral len_new_data)
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



closeBotan :: BotanPad -> IO ()
closeBotan botan_pad =
    do
      tls_channel_fptr <- readIORef (botan_pad ^. tlsChannel_BP)
      withForeignPtr tls_channel_fptr $ \ tls_channel_ptr -> do
          maybe_gotit <- tryTakeMVar (botan_pad ^. active_BP)
          case maybe_gotit of
              Just _ -> do
                  -- Let's forbid libbotan for sending more output
                  _ <- tryPutMVar (botan_pad ^. problem_BP) (IOProblem  NoMoreDataException)
                  _ <- tryPutMVar (botan_pad ^. dataCame_BP) ()

                  alert_is_fatal <- iocba_alert_is_fatal tls_channel_ptr
                  either_sent <- if alert_is_fatal == 0 then
                      -- Cleanly close tls, if nobody else is writing there
                      nice_close tls_channel_ptr
                    else
                      -- No need to be nice if there was a fatal alert
                      return $ Right ()

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

                  -- Delete the channel. We had an idiom using the finalizers on the
                  -- foreign pointer, but somehow we were getting to this code *after*
                  -- those finalizers ran
                  iocba_delete_tls_server_channel' tls_channel_ptr

                  -- Somebody may be waiting for a protocol selected report,
                  -- dissapoint them duly.
                  _ <- tryPutMVar (botan_pad ^. selectedProtocol_BP) (E.throw $ TLSEncodingIssue)
                  return ()

              Nothing ->
                  -- Thing already closed, don't do it again
                  return ()
  where

    nice_close tls_channel_ptr = do
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
                          return last_message

      -- Send the last message, if possible at all... otherwise just ignore the
      -- exception
        E.try $ do
            (botan_pad ^. encryptedSide_BP . pushAction_IOC) (LB.fromStrict last_msg)


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
          , _closeActionCalled_IOC = botan_pad' ^. (encryptedSide_BP . closeActionCalled_IOC)
            }


instance TLSContext BotanTLSContext BotanSession where
    newTLSContextFromMemory = newBotanTLSContextFromMemory
    newTLSContextFromCertFileNames = newBotanTLSContext
    unencryptTLSServerIO = unencryptChannelData
    getSelectedProtocol (BotanSession pad_ioref) = do
        botan_pad <- readIORef pad_ioref
        -- Notice that "sessionWasResumed" also uses selectedProtocol_BP
        a <- readMVar (botan_pad ^. selectedProtocol_BP)
        -- Force any botled exceptions to spring
        E.evaluate a

    enableSessionResumption (BotanTLSContext cpp_side) session_storage = do
        storage_crecord <- instanceToStorageCRecord session_storage
        withForeignPtr cpp_side $ \ c ->
            iocba_enable_sessions
                c
                (storage_crecord ^. pSave_Pre)
                (storage_crecord ^. pRemoveEntry_Pre)
                (storage_crecord ^. pLoad_Pre)
                (storage_crecord ^. pSessionLifetime_Pre)
                (storage_crecord ^. pEncryptionKey_Pre)
        return True

    sessionWasResumed (BotanSession pad_ioref) = do
        botan_pad <- readIORef pad_ioref
        -- Wait for a notification here...
        a <- readMVar (botan_pad ^. selectedProtocol_BP)
        -- Force any botled exceptions to spring
        _ <- E.evaluate a
        -- And now let's go for the interesting bits
        tls_channel_fptr <- readIORef (botan_pad ^. tlsChannel_BP)
        withForeignPtr tls_channel_fptr $ \ tls_channel_ptr -> do
            resumed_value <- iocba_session_was_resumed
                tls_channel_ptr
            return $ resumed_value > 0



botanTLS :: Proxy BotanTLSContext
botanTLS = Proxy
