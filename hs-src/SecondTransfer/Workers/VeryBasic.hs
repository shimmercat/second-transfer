{-# LANGUAGE OverloadedStrings #-}

module Rede.Workers.VeryBasic(
    veryBasic
    ,bad404ResponseData
    ,bad404ResponseHeaders
    ) where 


import qualified Data.ByteString as B
import           Data.Conduit
import           Data.ByteString.Char8 (pack)



import Rede.MainLoop.CoherentWorker


trivialHeaders :: [(B.ByteString, B.ByteString)] 
trivialHeaders = [
    (":status", "200"),
    ("server",  "reh0m")
    ]


veryBasic :: CoherentWorker
veryBasic (headers, _) = do 
    let data_and_conclussion = yield "Hello world!"

    putStrLn "Got these headers: "
    print headers 

    return (trivialHeaders, [], data_and_conclussion)


bad404ResponseData :: B.ByteString 
bad404ResponseData = "404: ReH: Didn't find that"


bad404ResponseHeaders ::  Headers 
bad404ResponseHeaders =  [
                 (":status", "404")
                ,(":version", "HTTP/1.1")
                ,("content-length", (pack.show $ B.length bad404ResponseData))
                ,("content-type", "text/plain")
                ,("server", "ReHv0.0")
                ]