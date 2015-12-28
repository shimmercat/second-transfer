{-# LANGUAGE RankNTypes,
             FunctionalDependencies,
             PartialTypeSignatures,
             OverloadedStrings,
             ScopedTypeVariables,
             TemplateHaskell
             #-}
module SecondTransfer.TLS.CoreServer (
               -- * Simpler interfaces
               -- These functions are simple enough but don't work with controllable
               -- processes.
                 tlsServeWithALPN
               , tlsServeWithALPNNSSockAddr
               , tlsSessionHandler
               , tlsServeWithALPNUnderSOCKS5SockAddr

               -- * Interfaces for a pre-fork scenario
               -- The first part of the invocation opens and
               -- bind the socket, the second part does the accepting...
               , NormalTCPHold
               , tlsServeWithALPNNSSockAddr_Prepare
               , tlsServeWithALPNNSSockAddr_Do
               , Socks5Hold
               , tlsServeWithALPNUnderSOCKS5SockAddr_Prepare
               , tlsServeWithALPNUnderSOCKS5SockAddr_Do

               , coreListen

               -- * Utility
               , NamedAttendants
               , chooseProtocol

               -- * Conduit-based session management
               , coreItcli

       ) where

import           Control.Concurrent
import           Control.Monad.IO.Class                                    (liftIO)
import           Control.Lens                                              ( (^.), makeLenses, over, set )

import           Data.Conduit
-- import qualified Data.Conduit                                              as Con(yield)
import qualified Data.Conduit.List                                         as CL
import           Data.Typeable                                             (Proxy(..))
import           Data.List                                                 (elemIndex)
import           Data.Maybe                                                (fromMaybe, isJust)
import qualified Data.ByteString                                           as B
import           Data.ByteString.Char8                                     (pack, unpack)
import           Data.Int                                                  (Int64)

--import qualified Data.ByteString.Lazy                                      as LB
--import qualified Data.ByteString.Builder                                   as Bu

import qualified Network.Socket                                            as NS

import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.TLS.Types
import           SecondTransfer.IOCallbacks.SocketServer
import           SecondTransfer.IOCallbacks.WrapSocket                     (HasSocketPeer(..))

import           SecondTransfer.Socks5.Session                             (tlsSOCKS5Serve, initSocks5ServerState)
import           SecondTransfer.Socks5.Types                               (Socks5ConnectionCallbacks)
import           SecondTransfer.Exception                                  (forkIOExc)


data SessionHandlerState = SessionHandlerState {
    _liveSessions_S    ::  !Int64
  , _nextConnId_S      ::  !Int64
  , _connCallbacks_S   ::  ConnectionCallbacks
    }

makeLenses ''SessionHandlerState


-- | A simple Alias
type NamedAttendants = [(String, Attendant)]


-- | Convenience function to open a port and listen there for connections and
--   select protocols and so on.
tlsServeWithALPN ::   forall ctx session . (TLSContext ctx session)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Control and log connections
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> String                -- ^ Name of the network interface
                 -> NamedAttendants       -- ^ List of attendants and their handlers
                 -> Int                   -- ^ Port to listen for connections
                 -> IO ()
tlsServeWithALPN proxy  conn_callbacks cert_filename key_filename interface_name attendants interface_port = do
    listen_socket <- createAndBindListeningSocket interface_name interface_port
    coreListen proxy conn_callbacks cert_filename key_filename listen_socket tlsServe attendants

-- | Use a previously given network address
tlsServeWithALPNNSSockAddr ::   forall ctx session . (TLSContext ctx session)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Control and regulate SOCKS5 connections
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> NamedAttendants        -- ^ List of attendants and their handlers
                 -> IO ()
tlsServeWithALPNNSSockAddr proxy conn_callbacks  cert_filename key_filename sock_addr attendants = do
    listen_socket <- createAndBindListeningSocketNSSockAddr sock_addr
    coreListen proxy conn_callbacks cert_filename key_filename listen_socket tlsServe attendants

data NormalTCPHold   = NormalTCPHold ( IO () )

-- | The prefork way requires a first step where we create the sockets and then we listen on them...
--   This function is identical otherwise to the one without _Prepare. The real thing is done by the
--   one with _Do below...
tlsServeWithALPNNSSockAddr_Prepare ::   forall ctx session . (TLSContext ctx session)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Control and regulate SOCKS5 connections
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> NamedAttendants       -- ^ List of attendants and their handlers
                 -> IO NormalTCPHold
tlsServeWithALPNNSSockAddr_Prepare proxy conn_callbacks  cert_filename key_filename sock_addr attendants = do
    listen_socket <- createAndBindListeningSocketNSSockAddr sock_addr
    return . NormalTCPHold $ coreListen proxy conn_callbacks cert_filename key_filename listen_socket tlsServe attendants


-- | Actually listen, possibly at the other side of the fork.
tlsServeWithALPNNSSockAddr_Do :: NormalTCPHold  -> IO ()
tlsServeWithALPNNSSockAddr_Do (NormalTCPHold action) = action


tlsServeWithALPNUnderSOCKS5SockAddr ::   forall ctx session  . (TLSContext ctx session)
                 => Proxy ctx             -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Log and control of the inner TLS session
                 -> Socks5ConnectionCallbacks -- ^ Log and control the outer SOCKS5 session
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> NamedAttendants       -- ^ List of attendants and their handlers
                 -> [B.ByteString]        -- ^ Names of "internal" hosts
                 -> Bool                  -- ^ Should I forward connection requests?
                 -> IO ()
tlsServeWithALPNUnderSOCKS5SockAddr
    proxy
    conn_callbacks
    socks5_callbacks
    cert_filename
    key_filename
    host_addr
    attendants
    internal_hosts
    forward_no_internal = do
    let
        approver :: B.ByteString -> Bool
        approver name = isJust $ elemIndex name internal_hosts
    socks5_state_mvar <- newMVar initSocks5ServerState
    listen_socket <- createAndBindListeningSocketNSSockAddr host_addr
    coreListen
       proxy
       conn_callbacks
       cert_filename
       key_filename
       listen_socket
       (tlsSOCKS5Serve socks5_state_mvar socks5_callbacks approver forward_no_internal)
       attendants


-- | Opaque hold type
data Socks5Hold = Socks5Hold (IO ())


tlsServeWithALPNUnderSOCKS5SockAddr_Prepare ::   forall ctx session  . (TLSContext ctx session)
                 => Proxy ctx             -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Log and control of the inner TLS session
                 -> Socks5ConnectionCallbacks -- ^ Log and control the outer SOCKS5 session
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> NamedAttendants       -- ^ List of attendants and their handlers
                 -> [B.ByteString]        -- ^ Names of "internal" hosts
                 -> Bool                  -- ^ Should I forward connection requests?
                 -> IO Socks5Hold
tlsServeWithALPNUnderSOCKS5SockAddr_Prepare
    proxy
    conn_callbacks
    socks5_callbacks
    cert_filename
    key_filename
    host_addr
    attendants
    internal_hosts
    forward_no_internal = do
    let
        approver :: B.ByteString -> Bool
        approver name = isJust $ elemIndex name internal_hosts
    socks5_state_mvar <- newMVar initSocks5ServerState
    listen_socket <- createAndBindListeningSocketNSSockAddr host_addr
    return . Socks5Hold $ coreListen
       proxy
       conn_callbacks
       cert_filename
       key_filename
       listen_socket
       (tlsSOCKS5Serve socks5_state_mvar socks5_callbacks approver forward_no_internal)
       attendants


tlsServeWithALPNUnderSOCKS5SockAddr_Do :: Socks5Hold -> IO ()
tlsServeWithALPNUnderSOCKS5SockAddr_Do (Socks5Hold action) = action


tlsSessionHandler ::  (TLSContext ctx session, TLSServerIO encrypted_io, HasSocketPeer encrypted_io) =>
       MVar SessionHandlerState
       -> NamedAttendants
       ->  ctx
       ->  encrypted_io
       -> IO ()
tlsSessionHandler session_handler_state_mvar attendants ctx encrypted_io = do
    -- Have the handshake happen in another thread
    forkIOExc "tlsSessionHandler" $ do

      -- Get a new connection id
      (new_conn_id, live_now) <- modifyMVar session_handler_state_mvar $ \ s -> do
          let new_conn_id = s ^. nextConnId_S
              live_now_ = (s ^. liveSessions_S) + 1
              new_s = over nextConnId_S ( + 1 ) s
              new_new_s = set liveSessions_S live_now_ s
          return $  new_new_s `seq` (new_new_s, (new_conn_id, live_now_) )

      connection_callbacks <- withMVar session_handler_state_mvar $ \ s -> do
          return $ s ^. connCallbacks_S
      let
          log_events_maybe = connection_callbacks ^. logEvents_CoCa
          log_event :: ConnectionEvent -> IO ()
          log_event ev = case log_events_maybe of
              Nothing -> return ()
              Just c -> c ev
          wconn_id = ConnectionId new_conn_id
      sock_addr <- getSocketPeerAddress encrypted_io
      log_event (Established_CoEv sock_addr wconn_id live_now)
      session <- unencryptTLSServerIO ctx encrypted_io
      plaintext_io_callbacks_u <- handshake session :: IO IOCallbacks

      let
          instr = do
              (plaintext_io_callbacks_u ^. closeAction_IOC)
              log_event (Ended_CoEv wconn_id)
          plaintext_io_callbacks = set closeAction_IOC instr plaintext_io_callbacks_u

      maybe_sel_prot <- getSelectedProtocol session
      let maybe_attendant =
            case maybe_sel_prot of
                Just (_, prot_name) ->
                    lookup (unpack prot_name) attendants
                Nothing ->
                    lookup "" attendants
      case maybe_attendant of
          Just use_attendant ->
              use_attendant plaintext_io_callbacks
          Nothing -> do
              log_event (ALPNFailed_CoEv wconn_id)
              plaintext_io_callbacks ^. closeAction_IOC
    return ()


tlsSessionHandlerControllable ::  (TLSContext ctx session, TLSServerIO encrypted_io)
    => [(String, ControllableAttendant controller)]  ->  ctx ->  encrypted_io -> IO (Maybe controller)
tlsSessionHandlerControllable attendants ctx encrypted_io = do
    session <- unencryptTLSServerIO ctx encrypted_io
    plaintext_io_callbacks <- handshake session :: IO IOCallbacks
    maybe_sel_prot <- getSelectedProtocol session
    case maybe_sel_prot of
        Just (_, prot_name) -> do
            let
                Just  use_attendant = lookup (unpack prot_name) attendants
            x <- use_attendant plaintext_io_callbacks
            return . Just $ x

        Nothing -> do
            let
                maybe_attendant = lookup "" attendants
            case maybe_attendant of
                Just use_attendant -> do
                    x <- use_attendant plaintext_io_callbacks
                    return . Just $ x
                Nothing -> do
                    -- Silently do nothing, and close the connection
                    plaintext_io_callbacks ^. closeAction_IOC
                    return Nothing



chooseProtocol :: [(String, a)] ->  [B.ByteString] -> IO (Maybe Int)
chooseProtocol attendants proposed_protocols =
    let
        i_want_protocols = map (pack . fst) attendants
        chosen =
            foldl
                ( \ selected want_protocol ->
                    case (selected, elemIndex want_protocol proposed_protocols) of
                        ( Just a, _) -> Just a
                        (_,   Just idx) -> Just idx
                        (_,   _ ) -> Nothing
                )
                Nothing
                i_want_protocols
    in return chosen


coreListen ::
       forall a ctx session b . (TLSContext ctx session, TLSServerIO b, HasSocketPeer b)
     => (Proxy ctx )                      -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
     -> ConnectionCallbacks               -- ^ Functions to log and control behaviour of the server
     -> FilePath                          -- ^ Path to certificate chain
     -> FilePath                          -- ^ Path to PKCS #8 key
     -> a                                 -- ^ An entity that is used to fork new handlers
     -> ( a -> (b -> IO()) -> IO () )    -- ^ The fork-handling functionality
     -> [(String, Attendant)]             -- ^ List of attendants and their handlers
     -> IO ()
coreListen _ conn_callbacks certificate_filename key_filename listen_abstraction session_forker attendants =   do
     let
         state = SessionHandlerState {
             _liveSessions_S = 0
           , _nextConnId_S = 0
           , _connCallbacks_S = conn_callbacks
             }
     state_mvar <- newMVar state
     ctx <- newTLSContext (pack certificate_filename) (pack key_filename) (chooseProtocol attendants) :: IO ctx
     session_forker listen_abstraction (tlsSessionHandler state_mvar attendants ctx)


-- | A conduit that takes TLS-encrypted callbacks, creates a TLS server session on top of it, passes the resulting
--   plain-text io-callbacks to a chosen Attendant in the argument list, and passes up the controller of the attendant
--   so that it can be undone if needs come. This should be considered a toy API, as multiple handshake can not progress
--   simultaeneusly through Conduits, so a server using this would be blocked for the entire length of a TLS handshake
--   with a remote client .... :-(
coreItcli ::
         forall controller ctx session b . (TLSContext ctx session, TLSServerIO b)
     => ctx                                                      -- ^ Passing in a tls context already-built value allows for sharing a single
                                                                 --   context across multiple listening abstractions...
     -> [(String, ControllableAttendant controller)]             -- ^ List of attendants and their handlers
     -> Conduit b IO controller
coreItcli  ctx  controllable_attendants = do
    let
        monomorphicHandler :: [(String, ControllableAttendant controller)]  ->  ctx ->  b -> IO (Maybe controller)
        monomorphicHandler =  tlsSessionHandlerControllable
    CL.mapMaybeM  $  liftIO <$> monomorphicHandler controllable_attendants ctx
