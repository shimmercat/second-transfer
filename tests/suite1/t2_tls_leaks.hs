{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import           Control.Concurrent
import           Control.Lens                                                       ( (^.) )

import qualified Data.ByteString                                                    as B
import qualified Data.ByteString.Lazy                                               as LB

import           SecondTransfer.IOCallbacks.SocketServer
import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.TLS.Botan
import           SecondTransfer.Exception( NoMoreDataException(..), IOProblem)

#include "instruments.cpphs"

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
            on_finish _ = do
                REPORT_EVENT("well-closed")
                return Nothing
            on_another :: SomeException -> IO (Maybe B.ByteString)
            on_another e = do
                putStrLn . show $ e
                return Nothing
        maybe_data <- handle on_another . handle on_finish $  (fmap Just $ bepa True)
        case maybe_data of
            Just some_data -> do
                B.putStr $ some_data
                push . LB.fromStrict $ "R " `mappend` some_data `mappend` "--"
                work cb
            Nothing -> do
                return ()

main = do
    ctx <- newBotanTLSContext "_priv/cert.pem" "_priv/privkey.unencrypted-pkcs8.pem" protocolSelector
    listen_socket <- createAndBindListeningSocket "127.0.0.1" 8090
    tlsServe listen_socket (simpleEcho ctx)
