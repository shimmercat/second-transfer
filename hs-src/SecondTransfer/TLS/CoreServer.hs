{-# LANGUAGE RankNTypes,
             FunctionalDependencies,
             PartialTypeSignatures,
             OverloadedStrings,
             GADTs,
             ScopedTypeVariables,
             TemplateHaskell
             #-}
module SecondTransfer.TLS.CoreServer (
               -- * Simpler interfaces
                 flatAction
               , tlsServeWithALPNNSSockAddr
               --, tlsSessionHandler
               , tlsServeWithALPNUnderSOCKS5SockAddr

               -- * Interfaces for a pre-fork scenario
               -- The first part of the invocation opens and
               -- bind the socket, the second part does the accepting...
               , NormalTCPHold
               , tlsServeWithALPNNSSockAddr_Prepare
               , tlsServeWithALPNNSSockAddr_Do
               , Socks5Hold
               , AcceptOutcome (..)

               , tlsServeWithALPNUnderSOCKS5SockAddr_Prepare
               , tlsServeWithALPNUnderSOCKS5SockAddr_Do

               , coreListen

               -- * Utility
               , NamedAttendants
               , chooseProtocol

               -- * Conduit-based session management
               -- , coreItcli

       ) where

import           Control.Concurrent
import qualified Control.Exception                                         as E
--import           Control.Monad.IO.Class                                    (liftIO)
import           Control.Monad                                             (when)
import           Control.Lens                                              ( (^.), makeLenses, over, set )
-- import           GHC.Stack

--import           Data.Conduit
-- import qualified Data.Conduit                                              as Con(yield)
--import qualified Data.Conduit.List                                         as CL
import           Data.Typeable                                             (Proxy(..))
import           Data.List                                                 (elemIndex)
import           Data.Maybe                                                (-- fromMaybe,
                                                                            isJust)
import qualified Data.ByteString                                           as B
--import           Data.ByteString.Char8                                     (pack, unpack)
import           Data.Int                                                  (Int64)
import qualified Data.HashMap.Strict                                       as HS
-- import           Data.IORef

--import qualified Data.ByteString.Lazy                                      as LB
--import qualified Data.ByteString.Builder                                   as Bu

import qualified Network.Socket                                            as NS


import           SecondTransfer.MainLoop.Protocol
import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.TLS.Types
import           SecondTransfer.IOCallbacks.SocketServer
import           SecondTransfer.IOCallbacks.WrapSocket                     (
                                                                           HasSocketPeer(..)
                                                                           )

import           SecondTransfer.Socks5.Session                             (
                                                                           tlsSOCKS5Serve',
                                                                           initSocks5ServerState)
import           SecondTransfer.Socks5.Types                               (Socks5ConnectionCallbacks)
import           SecondTransfer.Exception                                  (forkIOExc, IOProblem,
                                                                            ignoreException, threadKilled)

import           SecondTransfer.Sessions.HashableSockAddr                  (hashableSockAddrFromNSSockAddr,
                                                                            HashableSockAddr
                                                                           )

--import           Debug.Trace                                               (traceStack)


data SessionHandlerState = SessionHandlerState {
    _liveSessions_S    ::  !Int64
  , _nextConnId_S      ::  !Int64
  , _connCallbacks_S   ::  ConnectionCallbacks
    }

makeLenses ''SessionHandlerState


-- | A fixed constant for how long can a TLS handshake last.
--   Let's set it at three seconds.
tlsHandshakeTimeout :: Int
tlsHandshakeTimeout = 3000000


-- | How many TLS handshakes can be in transit from the
--   same host at any given time
maxHandshakesInTransit :: Int
maxHandshakesInTransit = 8


-- | A simple Alias
type NamedAttendants = [(String, Attendant)]

data NoStore

instance TLSSessionStorage NoStore



-- | A special, limited session handler that will close the connection very quickly.
flatAction :: forall plain . (PlainTextIO plain, HasSocketPeer plain) =>
    MVar SessionHandlerState ->
    Attendant  ->
    plain  ->
    MVar InFlightRegistry
           -> IO ()
flatAction session_handler_state_mvar attendant sio inflight = do
    io_callbacks <- handshake sio
    _ <- forkIOExc "clientThreadRedirector" .
       ignoreException threadKilled () $ do

          -- Get a new connection id... this is a pretty safe block, exceptions should
          -- be uncommon here.
          (new_conn_id, live_now) <- modifyMVar session_handler_state_mvar $ \ s -> do
              let new_conn_id = s ^. nextConnId_S
                  live_now_ = (s ^. liveSessions_S) + 1
                  new_s = over nextConnId_S ( + 1 ) s
                  new_new_s = live_now_ `seq` set liveSessions_S live_now_ new_s
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

              report_minus_one_connection _mark = do
                  log_event (Ended_CoEv wconn_id)
                  -- putStrLn $ "mark:  " ++ _mark
                  modifyMVar_ session_handler_state_mvar $ \ s -> do
                      let
                          live_now_ = (s ^. liveSessions_S) - 1
                          new_new_s = set liveSessions_S live_now_ s
                      new_new_s `seq` return new_new_s

              limits_not_enabled = not $ connection_callbacks ^. ddosProtectionsEnabled_CoCa


          either_sock_addr <- E.try $ getSocketPeerAddress sio

          case either_sock_addr :: Either E.IOException NS.SockAddr of

              Left _exc -> do
                  report_minus_one_connection "no-peer"
                  -- Close the connection
                  io_callbacks ^. closeAction_IOC

              Right sock_addr -> do
                  let
                      hashable_addr = hashableSockAddrFromNSSockAddr sock_addr
                      connection_data = ConnectionData
                          {
                             _addr_CnD = hashable_addr
                           , _connId_CnD = wconn_id
                          }

                  -- If there are too many connections, drop this one
                  proceed <- case hashable_addr of
                      Just hashable_addr_strict -> do
                          connections_now <- withMVar inflight $ \ inflight_hm -> let
                              count = HS.lookupDefault 0 hashable_addr_strict inflight_hm
                            in return count
                          return $ connections_now < maxHandshakesInTransit

                      -- If I don't have a source address, proceed always. The lack
                      -- of source address can be due to some SOCKS5 or strange
                      -- transport weirdiness.
                      Nothing -> return True

                   -- Pass it to the attendant
                  if proceed || limits_not_enabled
                      then do
                          case hashable_addr of
                              Just hashable_addr_strict -> do
                                  modifyMVar_ inflight $ \ inflight_hm -> let
                                      count = HS.lookupDefault 0 hashable_addr_strict inflight_hm
                                      new_count = count + 1
                                      new_inflight = HS.insert hashable_addr_strict new_count inflight_hm
                                    in return new_inflight
                              Nothing -> return ()
                          E.finally
                               (attendant connection_data io_callbacks)
                               (case hashable_addr of
                                   Just hashable_addr_strict -> do
                                       modifyMVar_ inflight $ \ inflight_hm -> let
                                           count = HS.lookupDefault 0 hashable_addr_strict inflight_hm
                                           new_count = count - 1
                                           new_inflight = if new_count > 0
                                             then
                                               HS.insert hashable_addr_strict new_count inflight_hm
                                             else
                                               HS.delete hashable_addr_strict inflight_hm
                                         in return new_inflight

                                    -- If I don't have a source address, proceed always. The lack
                                    -- of source address can be due to some SOCKS5 or strange
                                    -- transport weirdiness.
                                   Nothing -> return ()
                               )
                      else do
                        -- Just close the connection, without adding a new one, but report
                        -- the event
                        plaintext_io_callbacks <- handshake sio
                        log_event (TooManyInHandshake_CoEv sock_addr)
                        plaintext_io_callbacks ^. closeAction_IOC
                        report_minus_one_connection "too-many-from-same"
    -- Let's create a thread killer for clients that hang along for too
    -- much time.
    _ <- forkIOExc "clientThreadKiller" $ do
        -- TODO: Good place to have a tunable.
        threadDelay 3000000
        (io_callbacks ^. closeAction_IOC)
    return ()


-- | Use a previously given network address
tlsServeWithALPNNSSockAddr ::   forall ctx session . (TLSContext ctx session)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Control and regulate SOCKS5 connections
                 -> B.ByteString              -- ^ String with contents of certificate chain
                 -> B.ByteString              -- ^ String with contents of PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> NamedAttendants        -- ^ List of attendants and their handlers
                 -> Attendant
                 -> IO ()
tlsServeWithALPNNSSockAddr proxy conn_callbacks  cert_filename key_filename sock_addr attendants flat_attendant = do
    listen_socket <- createAndBindListeningSocketNSSockAddr sock_addr
    -- Close the socket if need comes
    coreListen
        proxy
        conn_callbacks
        cert_filename
        key_filename
        listen_socket
        tls_serve --(tlsServe' closing)
        (Nothing :: Maybe NoStore)
        attendants
        flat_attendant
  where
    tls_serve =
        let
            x :: NS.Socket -> (AcceptOutcome SocketIOCallbacks TLSServerSocketIOCallbacks  -> IO () ) -> IO ()
            x = (tlsServe' closing)
        in x

    closing = case conn_callbacks ^. serviceIsClosing_CoCa of
        Just clbk -> clbk
        Nothing -> (return False)



data NormalTCPHold   = NormalTCPHold ( IO () )

-- | The prefork way requires a first step where we create the sockets and then we listen on them...
--   This function is identical otherwise to the one without _Prepare. The real thing is done by the
--   one with _Do below...
tlsServeWithALPNNSSockAddr_Prepare ::   forall ctx session resumption_store . (TLSContext ctx session, TLSSessionStorage resumption_store)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Control and regulate SOCKS5 connections
                 -> B.ByteString              -- ^ String with contents of certificate chain
                 -> B.ByteString              -- ^ String with contents of PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> Maybe resumption_store
                 -> IO NamedAttendants    -- ^ Will-be list of attendants and their handlers
                 -> IO NormalTCPHold
tlsServeWithALPNNSSockAddr_Prepare
                proxy
                conn_callbacks
                cert_filename
                key_filename
                sock_addr
                maybe_resumption_store
                make_attendants
  = do
    listen_socket <- createAndBindListeningSocketNSSockAddr sock_addr
    return . NormalTCPHold $ do
        attendants <- make_attendants
        coreListen
            proxy
            conn_callbacks
            cert_filename
            key_filename
            listen_socket
            tls_serve
            maybe_resumption_store
            attendants
            flat_attendant
  where
    flat_attendant :: Attendant
    flat_attendant _ _ =
        error "PhantomFlatAttendantCalled"

    tls_serve =
        let
            x :: NS.Socket -> (AcceptOutcome SocketIOCallbacks TLSServerSocketIOCallbacks  -> IO () ) -> IO ()
            x = (tlsServe' closing)
        in x

    closing = case conn_callbacks ^. serviceIsClosing_CoCa of
        Just clbk -> clbk
        Nothing -> (return False)


-- | Actually listen, possibly at the other side of the fork.
tlsServeWithALPNNSSockAddr_Do :: NormalTCPHold  -> IO ()
tlsServeWithALPNNSSockAddr_Do (NormalTCPHold action) = action


tlsServeWithALPNUnderSOCKS5SockAddr ::   forall ctx session  . (TLSContext ctx session)
                 => Proxy ctx             -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Log and control of the inner TLS session
                 -> Socks5ConnectionCallbacks -- ^ Log and control the outer SOCKS5 session
                 -> B.ByteString              -- ^ String with contents of certificate chain
                 -> B.ByteString              -- ^ String with contents of PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> NamedAttendants       -- ^ List of attendants and their handlers,
                 -> [B.ByteString]        -- ^ Names of "internal" hosts
                 -> Attendant             -- ^ Flat attendant
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
    flat_attendant
    forward_no_internal = do
    let
        approver :: B.ByteString -> Bool
        approver name = isJust $ elemIndex name internal_hosts
    socks5_state_mvar <- newMVar initSocks5ServerState
    listen_socket <- createAndBindListeningSocketNSSockAddr host_addr
    -- let
    --      handler_fn :: NS.Socket
    --          -> (Either AcceptErrorCondition b0 -> IO ()) -> IO ()
    --      handler_fn socket =
    --          tlsSOCKS5Serve socks5_state_mvar socks5_callbacks approver forward_no_internal
    coreListen
       proxy
       conn_callbacks
       cert_filename
       key_filename
       listen_socket
       (tlsSOCKS5Serve' socks5_state_mvar socks5_callbacks approver forward_no_internal)
       (Nothing :: Maybe NoStore)
       attendants
       flat_attendant

-- | Opaque hold type
data Socks5Hold = Socks5Hold (IO ())


tlsServeWithALPNUnderSOCKS5SockAddr_Prepare ::   forall ctx session  . (TLSContext ctx session)
                 => Proxy ctx             -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> ConnectionCallbacks   -- ^ Log and control of the inner TLS session
                 -> Socks5ConnectionCallbacks -- ^ Log and control the outer SOCKS5 session
                 -> B.ByteString              -- ^ String with contents of certificate chain
                 -> B.ByteString              -- ^ String with contents of PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> IO NamedAttendants    -- ^ List of attendants and their handlers, as it will be built
                 -> [B.ByteString]        -- ^ Names of "internal" hosts
                 -> Attendant             -- ^ Flat attendant
                 -> Bool                  -- ^ Should I forward connection requests?
                 -> IO Socks5Hold
tlsServeWithALPNUnderSOCKS5SockAddr_Prepare
    proxy
    conn_callbacks
    socks5_callbacks
    cert_pemfile_data
    key_pemfile_data
    host_addr
    make_attendants
    internal_hosts
    flat_attendant
    forward_no_internal = do
    let
        approver :: B.ByteString -> Bool
        approver name = isJust $ elemIndex name internal_hosts
    socks5_state_mvar <- newMVar initSocks5ServerState
    listen_socket <- createAndBindListeningSocketNSSockAddr host_addr
    return . Socks5Hold $ do
        attendants <- make_attendants
        coreListen
            proxy
            conn_callbacks
            cert_pemfile_data
            key_pemfile_data
            listen_socket
            (tlsSOCKS5Serve'
                 socks5_state_mvar
                 socks5_callbacks
                 approver
                 forward_no_internal)
            (Nothing :: Maybe NoStore)
            attendants
            flat_attendant


tlsServeWithALPNUnderSOCKS5SockAddr_Do :: Socks5Hold -> IO ()
tlsServeWithALPNUnderSOCKS5SockAddr_Do (Socks5Hold action) = action


type InFlightRegistry =HS.HashMap HashableSockAddr Int


tlsSessionHandler ::
       (TLSContext ctx session, TLSServerIO encrypted_io, HasSocketPeer encrypted_io) =>
       MVar SessionHandlerState
       -> NamedAttendants
       -> ctx
       -> encrypted_io
       -> MVar InFlightRegistry  -- ^ Just know how many sessions are open for ip addresses
       -> IO ()
tlsSessionHandler session_handler_state_mvar attendants ctx encrypted_io inflight = do
    -- Have the handshake happen in another thread
    _ <- forkIOExc "tlsSessionHandler" $ do

      -- Get a new connection id... this is a pretty safe block, exceptions should
      -- be uncommon here.
      (new_conn_id, live_now) <- modifyMVar session_handler_state_mvar $ \ s -> do
          let new_conn_id = s ^. nextConnId_S
              live_now_ = (s ^. liveSessions_S) + 1
              new_s = over nextConnId_S ( + 1 ) s
              new_new_s = live_now_ `seq` set liveSessions_S live_now_ new_s
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

          report_minus_one_connection _mark = do
              log_event (Ended_CoEv wconn_id)
              -- putStrLn $ "mark:  " ++ _mark
              modifyMVar_ session_handler_state_mvar $ \ s -> do
                  let
                      live_now_ = (s ^. liveSessions_S) - 1
                      new_new_s = set liveSessions_S live_now_ s
                  new_new_s `seq` return new_new_s

          limits_not_enabled = not $ connection_callbacks ^. ddosProtectionsEnabled_CoCa

      -- If the connection has been closed, we will get some sort of exception here.
      either_sock_addr <- E.try $ getSocketPeerAddress encrypted_io
      case either_sock_addr :: Either E.IOException NS.SockAddr of
          Left _exc -> do
              report_minus_one_connection "no-peer"
              -- Close the connection
              io_callbacks <- handshake encrypted_io
              io_callbacks ^. closeAction_IOC

          Right sock_addr -> do
              let
                  hashable_addr = hashableSockAddrFromNSSockAddr sock_addr
                  connection_data = ConnectionData
                      {
                         _addr_CnD = hashable_addr
                       , _connId_CnD = wconn_id
                      }

              -- If there are too many connections, drop this one
              proceed <- case hashable_addr of
                  Just hashable_addr_strict -> do
                      connections_now <- withMVar inflight $ \ inflight_hm -> let
                          count = HS.lookupDefault 0 hashable_addr_strict inflight_hm
                        in return count
                      return $ connections_now < maxHandshakesInTransit

                  -- If I don't have a source address, proceed always. The lack
                  -- of source address can be due to some SOCKS5 or strange
                  -- transport weirdiness.
                  Nothing -> return True

              if proceed || limits_not_enabled
                then do
                  case hashable_addr of
                      Just hashable_addr_strict -> do
                          modifyMVar_ inflight $ \ inflight_hm -> let
                              count = HS.lookupDefault 0 hashable_addr_strict inflight_hm
                              new_count = count + 1
                              new_inflight = HS.insert hashable_addr_strict new_count inflight_hm
                            in return new_inflight

                       -- If I don't have a source address, proceed always. The lack
                       -- of source address can be due to some SOCKS5 or strange
                       -- transport weirdiness.
                      Nothing -> return ()
                  E.finally
                      (continueToHandshake
                              log_event
                              sock_addr
                              wconn_id
                              live_now
                              ctx
                              encrypted_io
                              connection_callbacks
                              connection_data
                              report_minus_one_connection
                              attendants
                      )
                      (case hashable_addr of
                          Just hashable_addr_strict -> do
                              modifyMVar_ inflight $ \ inflight_hm -> let
                                  count = HS.lookupDefault 0 hashable_addr_strict inflight_hm
                                  new_count = count - 1
                                  new_inflight = if new_count > 0
                                    then
                                      HS.insert hashable_addr_strict new_count inflight_hm
                                    else
                                      HS.delete hashable_addr_strict inflight_hm
                                in return new_inflight

                           -- If I don't have a source address, proceed always. The lack
                           -- of source address can be due to some SOCKS5 or strange
                           -- transport weirdiness.
                          Nothing -> return ()
                      )
                else do
                  -- Just close the connection, without adding a new one, but report
                  -- the event
                  plaintext_io_callbacks <- handshake encrypted_io
                  log_event (TooManyInHandshake_CoEv sock_addr)
                  plaintext_io_callbacks ^. closeAction_IOC
                  report_minus_one_connection "too-many-from-same"


    -- Tell the upper layer that this in-flux socket has been passed to an
    -- attendant (and thus will be managed by the Tidal manager)
    return ()



continueToHandshake :: forall cipherio ctx session .
                       (TLSContext ctx session, TLSServerIO cipherio) =>
                       (ConnectionEvent -> IO ())
                       -> NS.SockAddr
                       -> ConnectionId
                       -> Int64
                       -> ctx
                       -> cipherio
                       -> ConnectionCallbacks
                       -> ConnectionData
                       -> (String -> IO () )
                       -> [(String, ConnectionData -> IOCallbacks -> IO ())]
                       -> IO ()
continueToHandshake
    log_event
    sock_addr
    wconn_id
    live_now
    ctx
    encrypted_io
    connection_callbacks
    connection_data
    report_minus_one_connection
    attendants
  = do
    log_event (Established_CoEv sock_addr wconn_id live_now)

    -- Let's decrypt stuff ... This function usually doesn't throw,
    -- as it mainly does channel setup and very little or none actual
    -- IO. TODO: But we may need to watch it for problems.
    session <- {-# SCC u1 #-} unencryptTLSServerIO ctx encrypted_io

    -- The handshake is more about setting up channels and actions, no
    -- actual TLS handshake is expected to happen here. In fact, this
    -- action and the unencryptTLSServerIO could be together. The
    -- reason to have them separated is the ctx argument above.
    plaintext_io_callbacks_u' <-  {-# SCC u2 #-} handshake session :: IO IOCallbacks

    -- Modulate the IO callbacks if that has been instructed.
    plaintext_io_callbacks_u <- case (connection_callbacks ^. blanketPlainTextIO_CoCa) of
        Nothing -> return plaintext_io_callbacks_u'
        Just u -> u connection_data plaintext_io_callbacks_u'

    -- Using a different MVar, don't change line below.
    close_reported_outer <- newMVar False

    let
        instr = do
            modifyMVar_ close_reported_outer $ \ close_reported_x -> do
                if (not close_reported_x) then  do
                    -- We can close just once
                    plaintext_io_callbacks_u ^. closeAction_IOC
                    report_minus_one_connection "normal-close"
                    return True
                  else
                    return close_reported_x

        plaintext_io_callbacks = set closeAction_IOC instr plaintext_io_callbacks_u

    -- Next point where things can go wrong: a handshake may never
    -- finish, and we may never get the protocol.
    -- Deep in Botan.hs this function is just waiting on an MVar for
    -- at least one thing: to have the TLS handshake finish and the
    -- protocol to be selected.
    -- Now let's allow for that to timeout.

    -- transit_state: odd numbers are bad, even numbers mean everything is going
    -- according to plans
    transit_state <- newMVar (0 :: Int)
    done_waiting <- newEmptyMVar

    _ <- forkIOExc "tlsSessionHandler.mayTimeOut" $ do
        either_maybe_sel_prot <-  E.try $  getSelectedProtocol session
        proceed <- modifyMVar transit_state $ \ i ->
            if i == 0
               then
                  return (2,True)
               else
                  return (i,False)
        when proceed $ do
            case either_maybe_sel_prot :: Either IOProblem HttpProtocolVersion of
                Right maybe_sel_prot ->  do
                    let maybe_attendant =
                          case maybe_sel_prot of
                              Http11_HPV ->
                                  lookup "http/1.1" attendants
                              Http2_HPV ->
                                  lookup "h2" attendants
                    case maybe_attendant of
                        Just use_attendant ->
                             {-# SCC u4 #-} use_attendant connection_data plaintext_io_callbacks
                        Nothing -> do
                            log_event (ALPNFailed_CoEv wconn_id)
                            plaintext_io_callbacks ^. closeAction_IOC
                    modifyMVar_ transit_state $ \ _ ->
                        return 4
                    putMVar done_waiting ()

                Left _exc -> do
                    plaintext_io_callbacks ^. closeAction_IOC
                    report_minus_one_connection "io-problem"
                    modifyMVar_ transit_state $ \ _ ->
                        return 5
                    putMVar done_waiting ()

    _ <- forkIOExc "tlsSessionHandler.doTimeOut" $ do
        threadDelay tlsHandshakeTimeout
        dosignal <- modifyMVar transit_state $ \ i ->
            if i == 0
              then do
                 return (1, True)
              else do
                 return (i, False)
        when dosignal $ do
             log_event (TLSHandshakeTimeOut_CoEv sock_addr)
             plaintext_io_callbacks ^. closeAction_IOC
             report_minus_one_connection "handshake-tiemout"
             putMVar done_waiting ()

    -- Wait for one of the two branches to arrive here: either the
    -- one that does I/O or the one that does waiting for a timeout.
    readMVar done_waiting
    return ()



chooseProtocol :: [(String, a)] ->  HttpProtocolVersion
chooseProtocol (("http/1.1" , _):_ ) = Http11_HPV
chooseProtocol (("h2", _):_ ) = Http2_HPV
chooseProtocol _ = Http11_HPV


coreListen ::
           forall a ctx session plain encrypted resumption_store .
           (TLSContext ctx session, PlainTextIO plain, TLSServerIO encrypted,
            HasSocketPeer encrypted, HasSocketPeer plain, TLSSessionStorage resumption_store)
         => (Proxy ctx )                      -- ^ This is a simple proxy type from Typeable that is used to select the type
                                              --   of TLS backend to use during the invocation
         -> ConnectionCallbacks               -- ^ Functions to log and control behaviour of the server
         -> B.ByteString                      -- ^ PEM-encoded certificate chain, in this string
         -> B.ByteString                      -- ^ PEM-encoded, un-encrypted PKCS #8 key in this string
         -> a                                 -- ^ An entity that is used to fork new handlers
         -> ( a -> (AcceptOutcome plain encrypted -> IO()) -> IO () )    -- ^ The fork-handling functionality
         -> Maybe resumption_store
         -> [(String, Attendant)]             -- ^ List of attendants and their handlers
         -> Attendant                         -- ^ Special attendant for connections coming "flat", i.e.
                                              --   which are not supposed to be wrapped in a TLS stack.
         -> IO ()
coreListen
         _
         conn_callbacks
         certificate_pemfile_data
         key_pemfile_data
         listen_abstraction
         session_forker
         resumption_store
         attendants
         flat_attendant
  =   do
     let
         state = SessionHandlerState {
             _liveSessions_S = 0
           , _nextConnId_S = 0
           , _connCallbacks_S = conn_callbacks
             }
     state_mvar <- newMVar state
     ctx <-
         newTLSContextFromMemory
             certificate_pemfile_data
             key_pemfile_data
             (chooseProtocol attendants) :: IO ctx
     inflight <- newMVar HS.empty
     _session_resumption_enabled <- case resumption_store of
         Just really_there     -> enableSessionResumption ctx really_there
         Nothing -> return False
     let
         tls_session_handler :: forall w io .
             (PlainTextIO io,  TLSServerIO w, HasSocketPeer w, HasSocketPeer io) =>
             AcceptOutcome io w -> IO ()
         tls_session_handler either_aerr =
             case either_aerr of
                 ErrorCondition_AOu connect_condition ->

                     case (conn_callbacks ^. logEvents_CoCa) of
                         Just lgfn -> lgfn $ AcceptError_CoEv connect_condition
                         Nothing -> return ()

                 Plain_AOu good ->
                     case (conn_callbacks ^. serviceIsClosing_CoCa) of
                         Nothing ->
                             flatAction state_mvar flat_attendant good inflight

                         Just clbk -> do
                             service_is_closing <- clbk
                             if not service_is_closing
                               then
                                 flatAction state_mvar flat_attendant good inflight
                               else do
                                 -- Close inmediately the connection, without doing
                                 -- anything else
                                 ioc <- handshake good
                                 ioc ^. closeAction_IOC


                 ForTLS_AOu good ->

                     case (conn_callbacks ^. serviceIsClosing_CoCa) of
                         Nothing ->
                             tlsSessionHandler state_mvar attendants ctx good inflight

                         Just clbk -> do
                             service_is_closing <- clbk
                             if not service_is_closing
                               then
                                 tlsSessionHandler state_mvar attendants ctx good inflight
                               else do
                                 -- Close inmediately the connection, without doing
                                 -- anything else
                                 ioc <- handshake good
                                 ioc ^. closeAction_IOC

     session_forker listen_abstraction tls_session_handler
