{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import           Control.Concurrent
import           Control.Lens                                                       ( (^.) )

import qualified Data.ByteString                                                    as B
import qualified Data.ByteString.Lazy                                               as LB

import           SecondTransfer.MainLoop.SocketServer
import           SecondTransfer.MainLoop.PushPullType
import           SecondTransfer.TLS.Botan
import           SecondTransfer.Exception( NoMoreDataException(..), IOProblem)


protocolSelector :: [B.ByteString] -> IO B.ByteString
protocolSelector prot_list = do
    putStrLn $ "ALPN offered: " ++ show prot_list
    return . head $ prot_list

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
            push = cb ^. pushAction_IOC
            on_finish :: IOProblem -> IO (Maybe B.ByteString)
            on_finish _ = return Nothing
        maybe_data <- catch (fmap Just $ bepa True) on_finish
        case maybe_data of
            Just some_data -> do
                B.putStr $ some_data
                push . LB.fromStrict $ "R " `mappend` some_data `mappend` "--"
                work cb
            Nothing -> do
                putStrLn "Connection closed"
                return ()

main = do
    ctx <- newBotanTLSContext "_priv/cert.pem" "_priv/privkey.unencrypted-pkcs8.pem" protocolSelector
    listen_socket <- createAndBindListeningSocket "127.0.0.1" 8090
    tlsServe listen_socket (simpleEcho ctx)
