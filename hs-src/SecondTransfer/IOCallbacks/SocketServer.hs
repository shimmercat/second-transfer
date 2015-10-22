{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.IOCallbacks.SocketServer(
                SocketIOCallbacks
              , TLSServerSocketIOCallbacks         (..)
              , socket_SS

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
import           SecondTransfer.Exception                           (NoMoreDataException(..))


-- | IOCallbacks around an active socket
data SocketIOCallbacks = SocketIOCallbacks {
    _socket_SS    :: NS.Socket
  , _callbacks_SS :: IOCallbacks
    }

makeLenses ''SocketIOCallbacks

instance IOChannels SocketIOCallbacks where
    handshake s = return ( s ^. callbacks_SS )

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


-- This function wraps an active socket (e.g., one where it is possible to send and receve data)
-- in something with a set of active callbacks
socketIOCallbacks :: NS.Socket -> IO SocketIOCallbacks
socketIOCallbacks socket = do
    let
        uhandler = ((\ _ -> E.throwIO NoMoreDataException ) :: E.SomeException -> IO a )

        push_action lazy_bs =
            E.catch
                (NSB.sendMany socket . LB.toChunks $ lazy_bs)
                uhandler
        -- Unfortunately we are forced to totally rely on sockets blocking or not nature
        -- TODO:  Check if ignoring the flag here becomes a problem.
        best_effort_pull_action _ = do
            datum <- E.catch (NSB.recv socket 4096) uhandler
            if B.length datum == 0
                then do
                   E.throwIO NoMoreDataException
                else
                   return datum
        close_action = do
            NS.shutdown socket NS.ShutdownBoth
            NS.close socket
    pull_action_wrapping <- newPullActionWrapping  best_effort_pull_action
    let
        pull_action = pullFromWrapping pull_action_wrapping
        io_callbacks = IOCallbacks {
            _pushAction_IOC           = push_action
          , _pullAction_IOC           = pull_action
          , _bestEffortPullAction_IOC = best_effort_pull_action
          , _closeAction_IOC          = close_action
            }
    return $ SocketIOCallbacks {
        _socket_SS = socket
      , _callbacks_SS = io_callbacks
        }
