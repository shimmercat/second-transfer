{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.IOCallbacks.SocketServer(
                SocketIOCallbacks
              , TLSServerSocketIOCallbacks         (..)

              , createAndBindListeningSocket
              , tcpServe
              , tlsServe
              , socketIOCallbacks
       ) where


---import           Control.Concurrent
import qualified Control.Exception                                  as E
import           Control.Lens                                       (makeLenses, (^.))

import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Lazy                               as LB
--import           Data.ByteString.Char8                              (pack, unpack)
--import           Data.List                                          (find)

-- import           System.Exit
--import           System.Posix.Signals

import qualified Network.Socket                                     as NS
import qualified Network.Socket.ByteString                          as NSB


import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.IOCallbacks.WrapSocket              (socketIOCallbacks, SocketIOCallbacks)
import           SecondTransfer.Exception                           (NoMoreDataException(..))


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
    NS.setSocketOption the_socket NS.RecvLowWater 8
    NS.setSocketOption the_socket NS.NoDelay 1
    NS.bind the_socket host_address
    return the_socket


-- | Simple TCP server. You must give a very short action, as the action
--   is run straight in the calling thread.
--   For a typical server, you would be doing a forkIO in the provided action.
tcpServe :: NS.Socket -> ( NS.Socket -> IO () ) -> IO ()
tcpServe  listen_socket action =
    do
        NS.listen listen_socket 20
        accept_loop listen_socket
  where
    accept_loop bind_socket = do
        (new_socket, _ ) <- NS.accept bind_socket
        E.catch
            (do
                action new_socket
            )
            -- TODO: Have to see this fail sometime
            ( (\ e -> putStrLn $ show e) :: E.SomeException -> IO ())
        accept_loop bind_socket


-- | Convenience function to create a TLS server. You are in charge of actually setting
--   up the TLS session, this only receives a type tagged with the IO thing...
tlsServe :: NS.Socket ->  ( TLSServerSocketIOCallbacks -> IO () ) -> IO ()
tlsServe listen_socket tls_action =
    tcpServe listen_socket tcp_action
  where
    tcp_action active_socket = do
        socket_io_callbacks <- socketIOCallbacks active_socket
        tls_action (TLSServerSocketIOCallbacks socket_io_callbacks)
