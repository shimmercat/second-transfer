{-# LANGUAGE OverloadedStrings #-}
import SecondTransfer(
	CoherentWorker
	, Footers
	, DataAndConclusion
	, tlsServeWithALPNAndFinishOnRequest
	, http2Attendant
	, FinishRequest(..)
	)
import SecondTransfer.Http2(
	  makeSessionsContext
	, defaultSessionsConfig
	)

import Data.Conduit
import Control.Concurrent         (threadDelay, forkIO)
import Control.Concurrent.MVar    

saysHello :: DataAndConclusion
saysHello = do 
	yield "Hello world!"
	-- No footers
	return []


helloWorldWorker :: CoherentWorker
helloWorldWorker request = return (
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
	finish <- newEmptyMVar
	forkIO $ do 
		threadDelay 1000000
		putMVar finish FinishRequest 
	let 
		http2_attendant = http2Attendant sessions_context helloWorldWorker
	tlsServeWithALPNAndFinishOnRequest
		"tests/support/servercert.pem"   -- Server certificate
		"tests/support/privkey.pem"      -- Certificate private key
		"127.0.0.1"                      -- On which interface to bind
		[
			("h2-14", http2_attendant),  -- Protocols present in the ALPN negotiation
			("h2",    http2_attendant)   -- they may be slightly different, but for this 
			                             -- test it doesn't matter.
		]
		8000
		finish