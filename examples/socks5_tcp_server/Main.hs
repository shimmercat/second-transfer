{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Concurrent 
import Control.Lens               ( (^.) )

import qualified                  Data.ByteString  as B
import qualified                  Data.ByteString.Lazy as LB

import SecondTransfer.Exception

import SecondTransfer.IOCallbacks.SocketServer
import SecondTransfer.IOCallbacks.Types

import SecondTransfer.Socks5.Types
import SecondTransfer.Socks5.Session

data TrivialResolver = TrivialAttendant

trivialAttendant :: Attendant
trivialAttendant cb =  do
    (cb ^. pushAction_IOC) "Hello world"
    let
        bepa = cb ^. bestEffortPullAction_IOC
        on_finish :: NoMoreDataException -> IO (Maybe B.ByteString)
        on_finish _ = return Nothing
        work = do
            maybe_data <- catch (fmap Just $ bepa True) on_finish
            case maybe_data of
                Just some_data -> do
                    B.putStr $ some_data
                    (cb ^. pushAction_IOC ) ( "--" `mappend` LB.fromStrict some_data `mappend` "ee" )
                    work
                Nothing -> do
                    putStrLn "Connection closed"
                    return ()
    forkIO  work
    return ()


instance Socks5Resolver TrivialResolver where

    s5Resolve _ addr port = do
        putStrLn . show $ addr 
        putStrLn . show $ port
        return . Just . makeAttendantDisruptible $ trivialAttendant

simpleEcho :: TLSServerIO a => a -> IO ()
simpleEcho pre_callbacks =  do
    callbacks <- handshake pre_callbacks
    serveSocks TrivialAttendant callbacks
    return ()

main = do
    listen_socket <- createAndBindListeningSocket "127.0.0.1" 8090
    tlsServe listen_socket simpleEcho

