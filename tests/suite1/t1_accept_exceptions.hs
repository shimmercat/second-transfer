{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Conduit
import           Control.Monad.IO.Class                             (liftIO)

import qualified Network.Socket                                     as NS
import qualified Network.Socket.ByteString                          as NSB
import           SecondTransfer.IOCallbacks.SocketServer

shouter :: Sink (NS.Socket, a) IO ()
shouter = do
    maybe_active_socket <- await
    --liftIO $ putStrLn "."
    case maybe_active_socket of
        Nothing -> return ()

        Just (active_socket,_) -> do
            liftIO $ NSB.sendAll active_socket "Hello world"
            shouter

main :: IO ()
main = do
    putStrLn "Hello"
    listen_socket <- createAndBindListeningSocket "127.0.0.1" 3037
    (tcpItcli listen_socket) $$ (shouter)
