{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.IOCallbacks.SocketServer(
                SocketIOCallbacks
              , TLSServerSocketIOCallbacks         (..)
              , createAndBindListeningSocket
              , socketIOCallbacks

              -- ** Socket server with callbacks
              , tcpServe
              , tlsServe

              -- ** Socket server with iterators
              , tcpItcli
              , tlsItcli
       ) where


import           Control.Concurrent                                 (threadDelay)
import qualified Control.Exception                                  as E
--import           Control.Lens                                       (makeLenses, (^.))
import           Control.Monad.IO.Class                             (liftIO)

import           Data.Conduit
import qualified Data.Conduit.List                                  as CL

--import qualified Data.ByteString                                    as B
--import qualified Data.ByteString.Lazy                               as LB
--import           Data.ByteString.Char8                              (pack, unpack)
--import           Data.List                                          (find)

-- import           System.Exit
--import           System.Posix.Signals
import           System.IO.Error                                    (ioeGetErrorString)

import qualified Network.Socket                                     as NS
--import qualified Network.Socket.ByteString                          as NSB


import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.IOCallbacks.WrapSocket              (socketIOCallbacks, SocketIOCallbacks)
--import           SecondTransfer.Exception                           (NoMoreDataException(..))
#include "instruments.cpphs"


-- | Simple alias to SocketIOCallbacks where we expect
--   encrypted contents
newtype TLSServerSocketIOCallbacks = TLSServerSocketIOCallbacks SocketIOCallbacks
    deriving IOChannels

instance TLSEncryptedIO TLSServerSocketIOCallbacks
instance TLSServerIO TLSServerSocketIOCallbacks

-- | Creates a listening socket at the provided network address (potentially a local interface)
--   and the given port number. It returns the socket. This result can be used by the function
--   tcpServe below
createAndBindListeningSocket :: String -> Int ->  IO NS.Socket
createAndBindListeningSocket hostname portnumber = do
    the_socket <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    addr_info0 : _ <- NS.getAddrInfo Nothing (Just hostname) Nothing
    addr_info1  <- return $ addr_info0 {
        NS.addrFamily = NS.AF_INET
      , NS.addrSocketType = NS.Stream
      , NS.addrAddress =
          (\ (NS.SockAddrInet _ a) -> NS.SockAddrInet (fromIntegral portnumber) a)
              (NS.addrAddress addr_info0)
        }
    host_address <- return $ NS.addrAddress addr_info1
    NS.setSocketOption the_socket NS.ReusePort 1
    NS.setSocketOption the_socket NS.RecvBuffer 32000
    NS.setSocketOption the_socket NS.SendBuffer 32000
    -- Linux honors the Low Water thingy below, and this setting is OK for HTTP/2 connections, but
    -- not very needed since the TLS wrapping will inflate the packet well beyond that size.
    -- See about this option here: http://stackoverflow.com/questions/8245937/whats-the-purpose-of-the-socket-option-so-sndlowat
    --
    -- NS.setSocketOption the_socket NS.RecvLowWater 8
    --
    NS.setSocketOption the_socket NS.NoDelay 1
    NS.bind the_socket host_address
    -- bound <- NS.isBound the_socket
    return the_socket


-- | Simple TCP server. You must give a very short action, as the action
--   is run straight in the calling thread.
--   For a typical server, you would be doing a forkIO in the provided action.
--   Do prefer to use tcpItcli directly.
tcpServe :: NS.Socket -> ( NS.Socket -> IO () ) -> IO ()
tcpServe  listen_socket action =
    tcpItcli listen_socket $$ CL.mapM_  (action . fst)


-- | Itcli is a word made from "ITerate-on-CLIents". This function makes an iterated
--   listen...
tcpItcli :: NS.Socket -> Source IO (NS.Socket, NS.SockAddr)
tcpItcli listen_socket =
    do
        liftIO $ NS.listen listen_socket 20
        LIO_REPORT_EVENT("listening")
        let
          -- TODO: System interrupts propagates freely!
          iterate' = do
              either_x <- liftIO . E.try $ NS.accept listen_socket
              case either_x of
                  Left e  | ioeGetErrorString e  == "resource exhausted" -> do
                              LIO_REPORT_EVENT("resource-exhausted")
                              liftIO $ threadDelay  (1000 * 1000)
                              iterate'
                          | ioeGetErrorString e == "signal" -> do
                              -- TODO: Some signals here should be processed differently, most likely!!
                              LIO_REPORT_EVENT("tcp-exit-on-signal")
                              return ()
                          | otherwise                                                                  -> liftIO $ do
                              -- TODO: Handle other interesting types of IOErrors in the loop above...
                              putStrLn $ "XXERR: " ++ ioeGetErrorString e
                              E.throwIO $ e
                  Right  (new_socket, sock_addr) -> do
                      yield (new_socket, sock_addr)
                      iterate'
        iterate'


-- | Convenience function to create a TLS server. You are in charge of actually setting
--   up the TLS session, this only receives a type tagged with the IO thing...
tlsServe :: NS.Socket ->  ( TLSServerSocketIOCallbacks -> IO () ) -> IO ()
tlsServe listen_socket tls_action =
    tcpServe listen_socket tcp_action
  where
    tcp_action active_socket = do
        socket_io_callbacks <- socketIOCallbacks active_socket
        tls_action (TLSServerSocketIOCallbacks socket_io_callbacks)


tlsItcli :: NS.Socket -> Source IO (TLSServerSocketIOCallbacks, NS.SockAddr)
tlsItcli listen_socket =
    fuse
        (tcpItcli listen_socket)
        ( CL.mapM
            $ \ (active_socket, address) -> do
                socket_io_callbacks <- socketIOCallbacks active_socket
                return (TLSServerSocketIOCallbacks socket_io_callbacks ,address)
        )
