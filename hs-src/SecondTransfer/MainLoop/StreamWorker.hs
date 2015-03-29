{-# LANGUAGE OverloadedStrings #-}
module Rede.MainLoop.StreamWorker (
    send404

	,StreamWorker
	,StreamInputToken
	,StreamOutputAction
	) where 


import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (pack)
import           Data.Conduit


import Rede.MainLoop.Tokens (
                             StreamOutputAction(..)
                             ,StreamInputToken
							 ,StreamOutputAction
							 ,StreamWorker
                             ,UnpackedNameValueList(..)
                             )


bad404ResponseData :: B.ByteString 
bad404ResponseData = "404: ReH: Didn't find that"


bad404ResponseHeaders ::  UnpackedNameValueList 
bad404ResponseHeaders =  UnpackedNameValueList [
                 (":status", "404")
                ,(":version", "HTTP/1.1")
                ,("content-length", (pack.show $ B.length bad404ResponseData))
                ,("content-type", "text/plain")
                ,("server", "ReHv0.0")
                ]


send404 :: ConduitM StreamInputToken StreamOutputAction IO ()
send404 = do 
    yield $ SendHeaders_SOA bad404ResponseHeaders
    yield $ SendData_SOA bad404ResponseData
    yield $ Finish_SOA