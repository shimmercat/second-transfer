{-# LANGUAGE OverloadedStrings #-}
import SecondTransfer(
    AwareWorker
    , Footers
    , DataAndConclusion
    , http2Attendant
    , http11Attendant
    , dropIncomingData
    , coherentToAwareWorker
    , FinishRequest(..)
    )
import SecondTransfer.Sessions(
      makeSessionsContext
    , defaultSessionsConfig
    )
import SecondTransfer.TLS.CoreServer
import SecondTransfer.TLS.Botan

import Data.Typeable              (Proxy)
import Data.Conduit
import Control.Concurrent         (threadDelay, forkIO)
import Control.Concurrent.MVar

saysHello :: DataAndConclusion
saysHello = do
    yield "Hello world!"
    -- No footers
    return []


helloWorldWorker :: AwareWorker
helloWorldWorker  = coherentToAwareWorker $ \ (_request_headers, _maybe_post_data) ->  do
    dropIncomingData _maybe_post_data
    return (
        [
            (":status", "200")
        ],
        [], -- No pushed streams
        saysHello
        )


-- For this program to work, it should be run from the top of
-- the developement directory.
main = do
    sessions_context <- makeSessionsContext defaultSessionsConfig
    let
        http2_attendant = http2Attendant sessions_context helloWorldWorker
        http11_attendant = http11Attendant sessions_context helloWorldWorker
        tls_session_proxy :: Proxy BotanTLSContext
        tls_session_proxy = error "Irrelevant"
    tlsServeWithALPN
        tls_session_proxy
        "_priv/cert.pem"                     -- Server certificate
        "_priv/privkey.unencrypted-pkcs8.pem"      -- Certificate private key
        "127.0.0.1"                      -- On which interface to bind
        [
            ("h2-14", http2_attendant),  -- Protocols present in the ALPN negotiation
            ("h2",    http2_attendant),   -- they may be slightly different, but for this
                                         -- test it doesn't matter.
            ("http/1.1", http11_attendant)
        ]
        8000
