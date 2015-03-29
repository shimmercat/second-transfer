
{-# LANGUAGE OverloadedStrings #-}

import Rede.MainLoop.Tls(
    tlsServeProtocols
    )


import qualified Data.ByteString.Lazy         as BL

import           Rede.SimpleHTTP1Response (exampleHTTP11Response)

import           Rede.MainLoop.PushPullType
import           Rede.MainLoop.Conduit
import           Rede.MainLoop.Tokens
import           Rede.MainLoop.ConfigHelp (getServedPort, getInterfaceName)

import           Rede.SpdyProtocol.Session(basicSession)
import           Rede.SpdyProtocol.TrivialTestWorker(FsWorkerServicePocket)
import           Rede.SpdyProtocol.Framing.ChunkProducer(chunkProducerHelper)


main :: IO ()
main = do
	port <- getServedPort
	iface <- getInterfaceName
	tlsServeProtocols [ 
		("spdy/3.1",spdyAttendant)
		,("http/1.1",httpAttendant) 
		] iface port


-- The "PushAction" is a callback that can pull bytes from 
-- some medium (a network socket, for example), while the 
-- PullAction is the opposite. Here I'm saying this: give me two 
-- callbacks for getting and sending data, and I will take care of 
-- the rest.
httpAttendant :: PushAction -> PullAction -> IO ()
httpAttendant push _ = 
    push $ BL.fromChunks [exampleHTTP11Response]


spdyAttendant :: PushAction -> PullAction -> IO () 
spdyAttendant push pull = do 
	-- TODO: This won't compile, after we added "params" to the 
	-- init service... the ones here are trivial
	fs_worker_service_pocket <- initService :: IO FsWorkerServicePocket
	activateSessionManager 	
	    id 
	    (basicSession fs_worker_service_pocket) 
	    push 
	    pull
 		chunkProducerHelper
