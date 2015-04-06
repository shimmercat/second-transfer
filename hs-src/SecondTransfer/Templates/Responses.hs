{-# LANGUAGE OverloadedStrings #-}
module SecondTransfer.Templates.Responses (
	StreamWorker
	,StreamInputToken
	,StreamOutputAction
	) where 


import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (pack)
import           Data.Conduit


import SecondTransfer.MainLoop.Tokens (
                             UnpackedNameValueList(..)
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

