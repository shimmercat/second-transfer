{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Concurrent                                  
import Control.Lens               ( (^.) )

import qualified                  Data.ByteString  as B

import SecondTransfer.MainLoop.SocketServer
import SecondTransfer.MainLoop.PushPullType

simpleEcho :: TLSServerIO a => a -> IO ()
simpleEcho pre_callbacks =  do
    forkIO $ do
        callbacks <- handshake pre_callbacks
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
    listen_socket <- createAndBindListeningSocket "127.0.0.1" 8090
    tlsServe listen_socket simpleEcho
