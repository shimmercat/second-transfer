{-# LANGUAGE OverloadedStrings #-}

module Rede.Workers.ResponseFragments (
    RequestMethod(..)

    ,simpleResponse
    ,getUrlFromHeaders
    ,ajaxResponseWithLength
    ,getMethodFromHeaders
    ) where 


import           Data.Conduit                 (Source, yield)
import qualified Data.Map                     as DM

import qualified Data.ByteString              as B
import           Data.ByteString.Char8        (pack)
import           Rede.MainLoop.CoherentWorker


-- TODO: Add the others....
data RequestMethod = 
    Get_RM
    |Put_RM
    |Post_RM


trivialHeaders :: Int -> Int -> [(B.ByteString, B.ByteString)] 
trivialHeaders status_code content_length = [
    (":status", (pack . show) status_code),
    ("reason", (DM.findWithDefault "" status_code code2Message) ),
    ("server",  "reh0m" ),
    ("content-type", "plain/text" ),
    ("access-control-allow-origin", "*"),
    ("content-length", (pack.show) content_length)
    ]

ajaxHeaders :: Int -> Int -> B.ByteString -> [(B.ByteString, B.ByteString)] 
ajaxHeaders status_code content_length content_type = [
    (":status", (pack . show) status_code ),
    ("content-length", (pack . show) content_length),
    ("content-type", content_type),
    ("access-control-allow-origin", "*"),
    ("server",  "reh0m" )
    ]


simpleResponse :: Int -> B.ByteString -> PrincipalStream
simpleResponse status_code text = 
    let 
        data_and_conclussion :: Source IO B.ByteString
        data_and_conclussion = yield text
    in (trivialHeaders status_code (B.length text), [], data_and_conclussion) 


ajaxResponseWithLength ::  Int -> Int -> B.ByteString -> B.ByteString -> PrincipalStream 
ajaxResponseWithLength status_code content_length content_type text = 
    let 
        data_and_conclussion :: Source IO B.ByteString
        data_and_conclussion = yield text
    in (ajaxHeaders status_code content_length content_type, [], data_and_conclussion) 


getUrlFromHeaders :: Headers -> B.ByteString
getUrlFromHeaders headers = let 
    (Just path)  = getHeaderFromFlatList headers ":path"
  in path


getMethodFromHeaders :: Headers -> RequestMethod
getMethodFromHeaders headers = let 
    (Just method) = getHeaderFromFlatList headers ":method"
  in case method of 
    m | m == "get" || m == "GET"   -> Get_RM
    m | m == "post"|| m == "POST"  -> Post_RM
    m | m == "put" || m == "PUT"   -> Put_RM


code2Message :: DM.Map Int B.ByteString
code2Message = DM.fromList [
    (200, " OK")
    ,(500, " INTERNAL SERVER ERROR")
    ,(404, " NOT FOUND")
    ]