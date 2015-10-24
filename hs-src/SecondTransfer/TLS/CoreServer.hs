{-# LANGUAGE RankNTypes, FunctionalDependencies, PartialTypeSignatures, OverloadedStrings, ScopedTypeVariables #-}
module SecondTransfer.TLS.CoreServer (
                 tlsServeWithALPN
               , tlsSessionHandler
               , tlsServeWithALPNUnderSOCKS5
               , coreListen
       ) where

import           Control.Concurrent

import           Data.Typeable                                             (Proxy(..))
import           Data.List                                                 (elemIndex)
import           Data.Maybe                                                (fromMaybe, isJust)
import qualified Data.ByteString                                           as B
import           Data.ByteString.Char8                                     (pack, unpack)
--import qualified Data.ByteString.Lazy                                      as LB
--import qualified Data.ByteString.Builder                                   as Bu
import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.TLS.Types
import           SecondTransfer.IOCallbacks.SocketServer

import           SecondTransfer.Socks5.Types                               (Socks5Resolver)
import           SecondTransfer.Socks5.Session                             (tlsSOCKS5Serve)
--import           SecondTransfer.Exception                                  ( NoMoreDataException(..), IOProblem)


-- | Convenience function to open a port and listen there for connections and
--   select protocols and so on.
tlsServeWithALPN ::
                 forall ctx session . (TLSContext ctx session)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> String                -- ^ Name of the network interface
                 -> [(String, Attendant)] -- ^ List of attendants and their handlers
                 -> Int                   -- ^ Port to listen for connections
--                 -> MVar FinishRequest    -- ^ Finish request event, write a value here to finish serving
                 -> IO ()
tlsServeWithALPN _ certificate_filename key_filename interface_name attendants interface_port  =
    do
        let
            i_want_protocols = map (pack . fst) attendants
            sel_protocol :: [B.ByteString] -> IO B.ByteString
            sel_protocol proposed_protocols = do
                let
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
                return chosen

        ctx <- newTLSContext (pack certificate_filename) (pack key_filename) sel_protocol :: IO ctx
        listen_socket <- createAndBindListeningSocket interface_name interface_port
        tlsServe listen_socket (tlsSessionHandler attendants ctx)


-- Experiment on polymorphism
tlsServeWithALPN2 ::   forall ctx session . (TLSContext ctx session)
                 => (Proxy ctx )          -- ^ This is a simple proxy type from Typeable that is used to select the type
                                          --   of TLS backend to use during the invocation
                 -> FilePath              -- ^ Path to certificate chain
                 -> FilePath              -- ^ Path to PKCS #8 key
                 -> String                -- ^ Name of the network interface
                 -> [(String, Attendant)] -- ^ List of attendants and their handlers
                 -> Int                   -- ^ Port to listen for connections
--                 -> MVar FinishRequest    -- ^ Finish request event, write a value here to finish serving
                 -> IO ()
tlsServeWithALPN2 proxy  cert_filename key_filename interface_name attendants interface_port = do
    listen_socket <- createAndBindListeningSocket interface_name interface_port
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


tlsSessionHandler ::  (TLSContext ctx session, TLSServerIO encrypted_io) => [(String, Attendant)]  ->  ctx ->  encrypted_io -> IO ()
tlsSessionHandler attendants ctx encrypted_io = do
    forkIO $
        do
          session <- unencryptTLSServerIO ctx encrypted_io
          plaintext_io_callbacks <- handshake session :: IO IOCallbacks
          (_, prot_name) <- getSelectedProtocol session
          --let
          let
              Just  use_attendant = lookup (unpack prot_name) attendants
          use_attendant plaintext_io_callbacks
    return ()


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
coreListen _ certificate_filename key_filename listen_abstraction session_forker attendants =
    do
        let
            i_want_protocols = map (pack . fst) attendants
            sel_protocol :: [B.ByteString] -> IO B.ByteString
            sel_protocol proposed_protocols = do
                let
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
                return chosen

        ctx <- newTLSContext (pack certificate_filename) (pack key_filename) sel_protocol :: IO ctx
        session_forker listen_abstraction (tlsSessionHandler attendants ctx)
