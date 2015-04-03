{-|
Module      : SecondTransfer
Description : A library for implementing HTTP\/2 servers supporting streaming requests and responses.
Copyright   : (c) Alcides Viamontes Esquivel, 2015
License     : BSD
Maintainer  : alcidesv@zunzun.se
Stability   : experimental
Portability : POSIX

This library implements enough of the HTTP/2  to build 
compliant HTTP/2 servers. The library

  * Is concurrent, meaning that you can use amazing Haskell lightweight threads to 
    process the requests. 

  * Obeys HTTP/2 flow control aspects.

  * And gives you freedom to (ab)use the HTTP/2 protocol in all the ways envisioned 
    by the standard. In particular you should be able to process streaming requests 
    (long uploads in POST or PUT requests) and to deliver streaming responses. You
    should even be able to do both simultaneously. 

Setting up TLS for HTTP/2 correctly is enough of a shore, so I have bundled here the
TLS setup logic. 

Frame encoding and decoding is done with 
Kazu Yamamoto's <http://hackage.haskell.org/package/http2 http2> package. 

Here is how you create a very basic HTTP/2 webserver:

@
{-# LANGUAGE OverloadedStrings #-}
import SecondTransfer(
    CoherentWorker
    , DataAndConclusion
    , tlsServeWithALPN
    , http2Attendant
    )

import Data.Conduit


saysHello :: 'DataAndConclusion'
saysHello = do 
    yield "Hello world!\\ns"
    -- No footers
    return []


helloWorldWorker :: 'CoherentWorker'
helloWorldWorker request = return (
    [
        (":status", "200")
    ],
    [], -- No pushed streams
    saysHello
    )


-- For this program to work, it should be run from the top of 
-- the developement directory, so that it has access to the toy 
-- certificates and keys defined there. 
main = do 
    'tlsServeWithALPN'
        "tests\/support\/servercert.pem"   -- Server certificate
        "tests\/support\/privkey.pem"      -- Certificate private key
        "127.0.0.1"                      -- On which interface to bind
        [
            ("h2-14", http2_attendant),  -- Protocols present in the ALPN negotiation
            ("h2",    http2_attendant)   -- they may be slightly different, but for this 
                                         -- test it doesn't matter.
        ]
        8000
  where 
    http2_attendant = http2Attendant helloWorldWorker
@

`CoherentWorker` is the basic callback function that you need to implement. 
The callback is used to handle all requests to the server on a given negotiated ALPN 
protocol. If you need routing functionality (and you most certainly will need it), you need
to build that functionality yourself or use one of the many Haskell libraries to that
end. 

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
    --   tune of  HTTP/2. That is, it should be able to take
    --   headers request first, and then a source of data coming in the 
    --   request (for example, POST data). Even before exhausting the source, 
    --   the coherent worker can post the response headers, and then create 
    --   its source for the response data. A coherent worker can also present
    --   create streams to push to the client. 
	  Headers
    , Request
    , Footers
    , CoherentWorker
    , PrincipalStream
    , PushedStreams
    , PushedStream
    , DataAndConclusion
    , InputDataStream
    , FinalizationHeaders
    
    -- * Basic utilities for  HTTP/2 servers
    ,Attendant
    ,PullAction
    ,PushAction
    ,CloseAction
    ,http2Attendant
    ,IOProblem
    ,GenericIOProblem
    -- * High level OpenSSL functions. 
    -- 
    -- | Use these functions to create your TLS-compliant 
    --   HTTP/2 server in a snap.
    ,tlsServeWithALPN
    ,tlsServeWithALPNAndFinishOnRequest

    ,TLSLayerGenericProblem(..)
    ,FinishRequest(..)
	) where 

import SecondTransfer.MainLoop.CoherentWorker 
import SecondTransfer.MainLoop
import SecondTransfer.Http2.MakeAttendant(http2Attendant)