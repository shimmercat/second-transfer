{-# LANGUAGE RankNTypes, FunctionalDependencies, PartialTypeSignatures, OverloadedStrings, ScopedTypeVariables #-}
module SecondTransfer.TLS.CoreServer (
               -- * Simpler interfaces
               -- These functions are simple enough but don't work with controllable
               -- processes.
                 tlsServeWithALPN
               , tlsServeWithALPNNSSockAddr
               , tlsSessionHandler
               , tlsServeWithALPNUnderSOCKS5
               , tlsServeWithALPNUnderSOCKS5SockAddr
               , coreListen

               -- * Utility
               , chooseProtocol

               -- * Conduit-based session management
               , coreItcli

       ) where

import           Control.Concurrent                                        (forkIO)
import           Control.Monad.IO.Class                                    (liftIO)
import           Control.Lens                                              ( (^.) )

import           Data.Conduit
-- import qualified Data.Conduit                                              as Con(yield)
import qualified Data.Conduit.List                                         as CL
import           Data.Typeable                                             (Proxy(..))
import           Data.List                                                 (elemIndex)
import           Data.Maybe                                                (fromMaybe, isJust)
import qualified Data.ByteString                                           as B
import           Data.ByteString.Char8                                     (pack, unpack)

--import qualified Data.ByteString.Lazy                                      as LB
--import qualified Data.ByteString.Builder                                   as Bu

import qualified Network.Socket                                            as NS

import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.TLS.Types
import           SecondTransfer.IOCallbacks.SocketServer

import           SecondTransfer.Socks5.Session                             (tlsSOCKS5Serve)
--import           SecondTransfer.Exception                                  ( NoMoreDataException(..), IOProblem)


-- | Convenience function to open a port and listen there for connections and
--   select protocols and so on.

-- Experiment on polymorphism
tlsServeWithALPN ::   forall ctx session . (TLSContext ctx session)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> String                -- ^ Name of the network interface
                 -> [(String, Attendant)] -- ^ List of attendants and their handlers
                 -> Int                   -- ^ Port to listen for connections
--                 -> MVar FinishRequest    -- ^ Finish request event, write a value here to finish serving
                 -> IO ()
tlsServeWithALPN proxy  cert_filename key_filename interface_name attendants interface_port = do
    listen_socket <- createAndBindListeningSocket interface_name interface_port
    coreListen proxy cert_filename key_filename listen_socket tlsServe attendants

-- | Use a previously given network address
tlsServeWithALPNNSSockAddr ::   forall ctx session . (TLSContext ctx session)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                  -> [(String, Attendant)] -- ^ List of attendants and their handlers
 --                 -> MVar FinishRequest    -- ^ Finish request event, write a value here to finish serving
                 -> IO ()
tlsServeWithALPNNSSockAddr proxy  cert_filename key_filename sock_addr attendants = do
    listen_socket <- createAndBindListeningSocketNSSockAddr sock_addr
    coreListen proxy cert_filename key_filename listen_socket tlsServe attendants


tlsServeWithALPNUnderSOCKS5 ::   forall ctx session  . (TLSContext ctx session)
                 => Proxy ctx             -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> String                -- ^ Name of the network interface
                 -> [(String, Attendant)] -- ^ List of attendants and their handlers
                 -> Int                   -- ^ Port to listen for connections
                 -> [B.ByteString]        -- ^ Names of "internal" hosts.
                 -> IO ()
tlsServeWithALPNUnderSOCKS5 proxy  cert_filename key_filename interface_name attendants interface_port internal_hosts = do
    let
        approver :: B.ByteString -> Bool
        approver name = isJust $ elemIndex name internal_hosts
    listen_socket <- createAndBindListeningSocket interface_name interface_port
    coreListen proxy cert_filename key_filename listen_socket (tlsSOCKS5Serve approver) attendants


tlsServeWithALPNUnderSOCKS5SockAddr ::   forall ctx session  . (TLSContext ctx session)
                 => Proxy ctx             -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> NS.SockAddr           -- ^ Address to bind to
                 -> [(String, Attendant)] -- ^ List of attendants and their handlers
                 -> [B.ByteString]        -- ^ Names of "internal" hosts.
                 -> IO ()
tlsServeWithALPNUnderSOCKS5SockAddr proxy  cert_filename key_filename host_addr attendants  internal_hosts = do
    let
        approver :: B.ByteString -> Bool
        approver name = isJust $ elemIndex name internal_hosts
    listen_socket <- createAndBindListeningSocketNSSockAddr host_addr
    coreListen proxy cert_filename key_filename listen_socket (tlsSOCKS5Serve approver) attendants

tlsSessionHandler ::  (TLSContext ctx session, TLSServerIO encrypted_io) => [(String, Attendant)]  ->  ctx ->  encrypted_io -> IO ()
tlsSessionHandler attendants ctx encrypted_io = do
    -- Have the handshake happen in another thread
    forkIO $ do
      session <- unencryptTLSServerIO ctx encrypted_io
      plaintext_io_callbacks <- handshake session :: IO IOCallbacks
      maybe_sel_prot <- getSelectedProtocol session
      case maybe_sel_prot of
          Just (_, prot_name) -> do
              let
                  Just  use_attendant = lookup (unpack prot_name) attendants
              use_attendant plaintext_io_callbacks
              return ()
          Nothing -> do
              let
                  maybe_attendant = lookup "" attendants
              case maybe_attendant of
                  Just use_attendant ->
                      use_attendant plaintext_io_callbacks
                  Nothing ->
                      -- Silently do nothing, and close the connection
                      plaintext_io_callbacks ^. closeAction_IOC
              return ()
    return ()


tlsSessionHandlerControllable ::  (TLSContext ctx session, TLSServerIO encrypted_io) => [(String, ControllableAttendant controller)]  ->  ctx ->  encrypted_io -> IO (Maybe controller)
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



chooseProtocol :: [(String, a)] ->  [B.ByteString] -> IO B.ByteString
chooseProtocol attendants proposed_protocols =
    let
        i_want_protocols = map (pack . fst) attendants
        chosen = fromMaybe "http/1.1" $
            foldl
                  ( \ selected want_protocol ->
                         case (selected, elemIndex want_protocol proposed_protocols) of
                             ( Just a, _) -> Just a
                             (_,   Just _) -> Just want_protocol
                             (_,   _ ) -> Nothing
                  )
                  Nothing
                  i_want_protocols
    in return chosen


coreListen ::
       forall a ctx session b . (TLSContext ctx session, TLSServerIO b)
     => (Proxy ctx )                      -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
     -> FilePath                          -- ^ Path to certificate chain
     -> FilePath                          -- ^ Path to PKCS #8 key
     -> a                                 -- ^ An entity that is used to fork new handlers
     -> ( a -> (b -> IO()) -> IO () )    -- ^ The fork-handling functionality
     -> [(String, Attendant)]             -- ^ List of attendants and their handlers
     -> IO ()
coreListen _ certificate_filename key_filename listen_abstraction session_forker attendants =   do
     ctx <- newTLSContext (pack certificate_filename) (pack key_filename) (chooseProtocol attendants) :: IO ctx
     session_forker listen_abstraction (tlsSessionHandler attendants ctx)


-- | A conduit that takes TLS-encrypted callbacks, creates a TLS server session on top of it, passes the resulting
--   plain-text io-callbacks to a chosen Attendant in the argument list, and passes up the controller of the attendant
--   so that it can be undone if needs come. This should be considered a toy API, as multiple handshake can not progress
--   simultaeneusly through Conduits :-(
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
