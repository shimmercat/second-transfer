{-|
Module      : SecondTransfer
Copyright   : (c) Alcides Viamontes Esquivel, 2015
License     : BSD
Maintainer  : alcidesv@zunzun.se
Stability   : experimental
Portability : POSIX

This library implements enough of the HTTP/2  to build 
compliant HTTP/2 servers. It also implements enough of 
HTTP/1.1 so you can actually use it to build polyglot web-servers.

For HTTP/2, frame encoding and decoding is done with 
Kazu Yamamoto's <http://hackage.haskell.org/package/http2 http2> package.
This library just takes care of making sense of sent and received
frames.

You can find more detailed information about this library at the page 
<https://www.httptwo.com/second-transfer/>.

The library

  * Is concurrent, meaning that you can use amazing Haskell lightweight threads to 
    process the requests. 

  * Obeys HTTP/2 flow control aspects, when talking HTTP/2.

  * And gives you freedom to (ab)use the HTTP/2 protocol in all the ways envisioned 
    by the standard. In particular you should be able to process streaming requests 
    (long uploads in POST or PUT requests) and to deliver streaming responses. You
    should even be able to do both simultaneously. 

Setting up TLS for HTTP/2 correctly is a shore, so I have bundled here the
TLS setup logic. Before you read any further, ATTENTION: enable always the threaded 
ghc runtime in your final programs if you want TLS to work.


Here is how you create a very basic HTTP/2 webserver:

@
{-# LANGUAGE OverloadedStrings #-}
import SecondTransfer(
    CoherentWorker
    , Footers
    , DataAndConclusion
    , tlsServeWithALPN
    , http2Attendant
    , http11Attendant
    )
import SecondTransfer.Sessions(
      makeSessionsContext
    , defaultSessionsConfig
    )

import Data.Conduit


saysHello :: DataAndConclusion
saysHello = do 
    -- The data in each yield will be automatically split across multiple 
    -- data frames if needed, so you can yield a large block of contents here
    -- if you wish. 
    -- If you do multiple yields, no data will be left buffered between them, 
    -- so that you can for example implement a chat client in a single HTTP/2 stream.
    -- Pity browsers hardly support that.
    yield "Hello world!"
    -- No footers
    return []


helloWorldWorker :: CoherentWorker
helloWorldWorker (_request_headers, _maybe_post_data) = do 
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
    tlsServeWithALPN
        "tests\/support\/servercert.pem"   -- Server certificate
        "tests\/support\/privkey.pem"      -- Certificate private key
        "127.0.0.1"                      -- On which interface to bind
        [
            ("no-protocol", http11_attendant), -- The first protocol in the list is used when 
                                               -- when no ALPN negotiation happens, and the
                                               -- name is really a filler.
            ("h2-14", http2_attendant),    -- Protocols present in the ALPN negotiation
            ("h2",    http2_attendant),    -- they may be slightly different, but for this 
                                           -- test it doesn't matter.

            ("http/1.1", http11_attendant) -- Let's talk HTTP/1.1 if everything else fails.
        ]
        8000 
@

`CoherentWorker` is the type of the basic callback function that you need to implement. 
The callback is used to handle all requests to the server on a given negotiated ALPN 
protocol. If you need routing functionality (and you most certainly will need it), you need
to build that functionality inside the callback. 

The above program uses a test certificate by a fake certificate authority. The certificate
is valid for the server name ("authority", in HTTP\/2 lingo) www.httpdos.com. So, in order
for the above program to run, you probably need to add an alias to your \/etc\/hosts file. 
You also need very up-to-date versions of OpenSSL (I'm using OpenSSL 1.0.2) to be compliant
with the cipher suites demanded by HTTP\/2. The easiest way to test the above program is using
a fairly recent version of <http://curl.haxx.se/ curl>. If everything is allright, 
you should be able to do:

@
   $ curl -k --http2 https://www.httpdos.com:8000/
   Hello world!
@

-}
module SecondTransfer(

    -- * Types related to coherent workers
    --
    -- | A coherent worker is an abstraction that can dance at the 
    --   tune of  HTTP/2 and HTTP/1.1. That is, it should be able to take
    --   headers from a request first, then a source of data coming in the 
    --   request (for example, POST data). Even before exhausting the source, 
    --   the coherent worker can post the response headers and
    --   its source for the response data. A coherent worker can also present
    --   streams to push to the client. 
      Headers
    , HeaderName
    , HeaderValue
    , Header
    , Request
    , Footers
    , CoherentWorker
    , PrincipalStream
    , PushedStreams
    , PushedStream
    , DataAndConclusion
    , InputDataStream
    , FinalizationHeaders
    
    -- ** How to couple bidirectional data channels to sessions
    ,Attendant
    ,http11Attendant
    ,http2Attendant

#ifndef DISABLE_OPENSSL_TLS
    -- * High level OpenSSL functions. 
    -- 
    -- | Use these functions to create your TLS-compliant 
    --   HTTP/2 server in a snap.
    ,tlsServeWithALPN
    ,tlsServeWithALPNAndFinishOnRequest

    ,TLSLayerGenericProblem(..)
    ,FinishRequest(..)
#endif -- DISABLE_OPENSSL_TLS

    ,dropIncomingData
    -- * Logging 
    --
    -- | The library uses hslogger for its logging. Since logging is 
    -- expensive, most of the instrumentation needs to be activated 
    -- at compile time by activating the "debug" flag. And then you 
    -- need to configure the loggers. The function `enableConsoleLogging` 
    -- configures them to output a lot of information to standard output.
    ,enableConsoleLogging
    ) where 

import           SecondTransfer.Http1                   (http11Attendant)
import           SecondTransfer.Http2.MakeAttendant     (http2Attendant)
import           SecondTransfer.MainLoop
import           SecondTransfer.MainLoop.CoherentWorker
import           SecondTransfer.Types
import           SecondTransfer.Utils.DevNull           (dropIncomingData)
