{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving, ForeignFunctionInterface, Rank2Types, GADTs  #-}
module SecondTransfer.IOCallbacks.SocketServer(
                SocketIOCallbacks
              , TLSServerSocketIOCallbacks                          (..)
              , TLSAcceptResult
              , createAndBindListeningSocketNSSockAddr
              , socketIOCallbacks

              -- ** Socket server with callbacks
              , tcpServe
              , tcpServeWithSockAddr
              , tlsServe
              , tlsServe'
              , tlsLARSIServe

              -- ** Socket server with iterators
              , tcpItcli
              --, tlsItcli
       ) where



import qualified Control.Exception                                  as E
import           Control.Concurrent                                 hiding (yield)
import           Control.Monad                                      (unless, when)
import           Control.Monad.IO.Class                             (liftIO)

import           Data.Conduit
import           Data.Maybe                                         (isJust)
import qualified Data.Conduit.List                                  as CL


import           System.IO.Error                                    (ioeGetErrorString)
import           System.Environment
import           Foreign.C.Types                                    (CInt(..))


import qualified Network.Socket                                     as NS

import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.IOCallbacks.WrapLARSISocket         (
                                                                     larsiSocketIOCallbacks
                                                                    )
import           SecondTransfer.IOCallbacks.WrapSocket              (
                                                                     socketIOCallbacks,
                                                                     SocketIOCallbacks,
                                                                     HasSocketPeer(..),
                                                                     AcceptErrorCondition(..)
                                                                     )
import           SecondTransfer.Exception                           (BadAddressException(..))

#include "instruments.cpphs"

foreign import ccall unsafe "iocba_enable_fastopen" iocba_enable_fastopen ::
    CInt ->
    IO ()



-- | Simple alias to SocketIOCallbacks where we expect
--   encrypted contents
data TLSServerSocketIOCallbacks where
    TLSServerSocketIOCallbacks ::
         (IOChannels a, HasSocketPeer a) =>  a ->  TLSServerSocketIOCallbacks

instance IOChannels TLSServerSocketIOCallbacks where
    handshake (TLSServerSocketIOCallbacks x) = handshake x


type TLSAcceptResult = Either AcceptErrorCondition TLSServerSocketIOCallbacks

instance TLSEncryptedIO TLSServerSocketIOCallbacks
instance TLSServerIO TLSServerSocketIOCallbacks

instance HasSocketPeer TLSServerSocketIOCallbacks where
    getSocketPeerAddress (TLSServerSocketIOCallbacks s) = getSocketPeerAddress s


-- | Enables TCP Fast Open, if possible. At the moment though the ratio between
--   benefits, support and feasibility points towards disabling this.
enableFastOpen :: NS.Socket -> IO ()
enableFastOpen (NS.MkSocket sock_descr _ _ _ _) =
    iocba_enable_fastopen sock_descr



-- | Inside containers, we are having troubles with socket options... coudl be related
--   to all the setSocketOption options there down, worth a try.
shouldSkipSocketOptions :: IO Bool
shouldSkipSocketOptions =
  do
    -- Use a very cryptic environment variable name
    maybe_skip <- lookupEnv "IXPGPYAPEC_SKIP_SOCKET_OPTIONS"
    return $ isJust maybe_skip



-- | Same as above, but it takes a pre-built address
createAndBindListeningSocketNSSockAddr :: NS.SockAddr ->  IO NS.Socket
createAndBindListeningSocketNSSockAddr host_addr = do
    the_socket <- case host_addr of
        NS.SockAddrInet _ _ -> NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        NS.SockAddrUnix _ -> NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
        NS.SockAddrInet6 _ _ _ _ -> NS.socket NS.AF_INET6 NS.Stream NS.defaultProtocol
        _ -> error "NetworkAddressTypeNotHandled"
#ifndef WIN32
    NS.setSocketOption the_socket NS.ReusePort 1
    -- enableFastOpen the_socket
#endif
    NS.setSocketOption the_socket NS.RecvBuffer 64000
    NS.setSocketOption the_socket NS.SendBuffer 64000
    -- Linux honors the Low Water thingy below, and this setting is OK for HTTP/2 connections, but
    -- not very needed since the TLS wrapping will inflate the packet well beyond that size.
    -- See about this option here: http://stackoverflow.com/questions/8245937/whats-the-purpose-of-the-socket-option-so-sndlowat
    --
    -- NS.setSocketOption the_socket NS.RecvLowWater 8
    --
    NS.setSocketOption the_socket NS.NoDelay 1
    NS.bind the_socket host_addr
    NS.listen the_socket 20
    -- bound <- NS.isBound the_socket
    return the_socket


-- | Simple TCP server. You must give a very short action, as the action
--   is run straight in the calling thread.
--   For a typical server, you would be doing a forkIO in the provided action.
--   Do prefer to use tcpItcli directly.
tcpServe :: NS.Socket -> (IO Bool) -> (Either AcceptErrorCondition NS.Socket -> IO () ) -> IO ()
tcpServe  listen_socket closing action =
    tcpItcli listen_socket closing $$
       CL.mapM_
           (\ either_condition_or_pair ->
               case either_condition_or_pair of
                   Left condition -> action $ Left condition
                   Right (a,_b) -> action $ Right a
           )


-- | tcpServe but Also passing the address
tcpServeWithSockAddr :: NS.Socket -> (IO Bool) -> (Either AcceptErrorCondition (NS.Socket, NS.SockAddr) -> IO () ) -> IO ()
tcpServeWithSockAddr  listen_socket closing action =
    tcpItcli listen_socket closing $$
       CL.mapM_
           (\ either_condition_or_pair ->
               case either_condition_or_pair of
                   Left condition -> action $ Left condition
                   Right x -> action $ Right x
           )


-- | Itcli is a word made from "ITerate-on-CLIents". This function makes an iterated
--   listen...
tcpItcli :: NS.Socket
         -> (IO Bool)
         -> Source IO (Either AcceptErrorCondition  (NS.Socket, NS.SockAddr) )
tcpItcli listen_socket closing =
    -- NOTICE: The messages below should be considered traps. Whenever one
    -- of them shows up, we have hit a new abnormal condition that should
    -- be learned from
    do
        let
          report_abnormality = do
              putStrLn "ERROR: TCP listen abstraction undone!!"
          -- TODO: System interrupts propagates freely!
          iterate' = do
              is_clossing <- liftIO closing
              unless is_clossing $ do
                  either_x <- liftIO . E.try $ NS.accept listen_socket
                  is_clossing' <- liftIO closing
                  case either_x of
                      Left e
                              | is_clossing' ->
                                  -- Just finish
                                  return ()
                              | ioeGetErrorString e  == "resource exhausted" -> do
                                  yield . Left $ ResourceExhausted_AEC
                                  iterate'
                              | s <- ioeGetErrorString e  -> do
                                  yield . Left $ Misc_AEC s
                                  iterate'
                      Right  (new_socket, sock_addr) -> do
                          yieldOr
                              (Right  (new_socket, sock_addr))
                              report_abnormality
                          iterate'

          watch = do
              is_closing <- closing
              if is_closing then
                  NS.close listen_socket
              else do
                  threadDelay $ 200*1000
                  watch

        _ <- liftIO $ forkIO watch

        iterate'


-- | Convenience function to create a TLS server. You are in charge of actually setting
--   up the TLS session, this only receives a type tagged with the IO thing...
--   Notice that the action should be short before actually forking towards something doing
--   the rest of the conversation. If you do the TLS handshake in this thread, you will be in
--   trouble when more than one client try to handshake simultaeneusly... ibidem if one of the
--   clients blocks the handshake.
tlsServe :: (IO Bool) -> NS.Socket -> ( TLSAcceptResult -> IO () ) -> IO ()
tlsServe closing listen_socket tls_action =
    tcpServe listen_socket closing tcp_action
  where
    tcp_action either_active_socket =
      case either_active_socket of
          Left condition ->
              tls_action $ Left condition

          Right active_socket ->
            do
              socket_io_callbacks <- socketIOCallbacks active_socket
              tls_action $
                  Right (TLSServerSocketIOCallbacks socket_io_callbacks)


tlsServe' :: forall a . PlainTextIO a =>
           (IO Bool) ->
           NS.Socket ->
           ( AcceptOutcome a TLSServerSocketIOCallbacks  -> IO () ) ->
           IO ()
tlsServe' closing listen_socket tls_action =
    tcpServe listen_socket closing tcp_action
  where
    tcp_action either_active_socket =
      case either_active_socket of
          Left condition ->
              tls_action $ ErrorCondition_AOu condition

          Right active_socket ->
            do
              socket_io_callbacks <- socketIOCallbacks active_socket
              tls_action $
                  ForTLS_AOu (TLSServerSocketIOCallbacks socket_io_callbacks)


-- | Accepts address prefixes.
tlsLARSIServe :: forall a . PlainTextIO a =>
           (IO Bool) ->
           NS.Socket ->
           ( AcceptOutcome a TLSServerSocketIOCallbacks  -> IO () ) ->
           IO ()
tlsLARSIServe closing listen_socket tls_action =
    tcpServe listen_socket closing tcp_action
  where
    tcp_action either_active_socket =
      case either_active_socket of
          Left condition ->
              tls_action $ ErrorCondition_AOu condition

          Right active_socket ->
            do
              socket_io_callbacks <- larsiSocketIOCallbacks active_socket
              tls_action $
                  ForTLS_AOu (TLSServerSocketIOCallbacks socket_io_callbacks)
