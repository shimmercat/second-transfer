{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Conduit
import           Control.Monad.IO.Class                             (liftIO)

import qualified Network.Socket                                     as NS
import qualified Network.Socket.ByteString                          as NSB
import           SecondTransfer.IOCallbacks.SocketServer
import           SecondTransfer.IOCallbacks.Types                   (ControllableAttendant, Damocles, makeAttendantDisruptible)
import           SecondTransfer.Types


saysHello :: DataAndConclusion
saysHello = do
    -- The data in each yield will be automatically split across multiple
    -- data frames if needed, so you can yield a large block of contents here
    -- if you wish.
    -- If you do multiple yields, no data will be left buffered between them,
    -- so that you can for example implement a chat client in a single HTTP/2 stream.
    -- Not that browsers support that.
    yield "Hello world!"
    -- The HTTP/2 protocol supports sending headers *after* stream data. So, you usually
    -- return an empty list to signal that you don't want any of those headers.
    -- In these docs, the post headers are often called "footers".
    return []

helloWorldWorker :: AwareWorker
helloWorldWorker (_request_headers, _maybe_post_data) = coherentToAwareWorker $ do
    dropIncomingData _maybe_post_data
    return (
        [
            (":status", "200")
        ],
        [], -- No pushed streams
        saysHello
        )

simpleControllableAttendant :: IO (ControllableAttendant Damobles)
simpleControllableAttendant = do
    sessions_context <- makeSessionsContext defaultSessionsConfig
    let
        http2_attendant = http2Attendant sessions_context helloWorldWorker
        disruptible = makeAttendantDisruptible . http2Attendant

main :: IO ()
main = do
    putStrLn "Hello"
    listen_socket <- createAndBindListeningSocket "127.0.0.1" 3037
    (tcpItcli listen_socket) $$ (shouter)
