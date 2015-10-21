{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Concurrent
import Control.Lens               ( (^.) )

import qualified                  Data.ByteString  as B

import SecondTransfer.MainLoop.SocketServer
import SecondTransfer.MainLoop.PushPullType
import SecondTransfer.TLS.Botan


simpleEcho :: TLSServerIO a => BotanTLSContext ->  a -> IO ()
simpleEcho ctx pre_callbacks =  do
    forkIO $ do
        botan_session <- unencryptChannelData ctx pre_callbacks
        callbacks <- handshake botan_session
        work callbacks
    return ()
  where
    work :: IOCallbacks -> IO ()
    work cb = do
        let
            bepa = cb ^. bestEffortPullAction_IOC
            on_finish :: NoMoreDataException -> IO (Maybe B.ByteString)
            on_finish _ = return Nothing
        maybe_data <- catch (fmap Just $ bepa True) on_finish
        case maybe_data of
            Just some_data -> do
                B.putStr $ some_data
                work cb
            Nothing -> do
                putStrLn "Connection closed"
                return ()

main = do
    ctx <- newBotanTLSContext "_priv/cert.pem" "_priv/privkey.unencrypted-pkcs8.pem"
    listen_socket <- createAndBindListeningSocket "127.0.0.1" 8090
    tlsServe listen_socket (simpleEcho ctx)
