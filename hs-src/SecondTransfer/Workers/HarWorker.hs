{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Rede.Workers.HarWorker(
    harCoherentWorker

    ) where 



import           Control.Monad.IO.Class

import qualified Data.ByteString              as B
import           Data.ByteString.Char8        (pack, unpack)
import           Data.Conduit
import qualified Data.Set                     as S

import           Control.Lens                 ((^.))
-- import qualified Control.Lens                 as L
import           System.Log.Logger
-- import           Control.Concurrent.MVar

import qualified Network.URI                  as U
-- import           Text.Printf


import           Rede.HarFiles.ServedEntry
import           Rede.MainLoop.CoherentWorker (CoherentWorker, PrincipalStream,
                                               getHeaderFromFlatList)
-- import           Rede.MainLoop.StreamWorker   (send404)
import           Rede.MainLoop.Tokens         (
                                               -- StreamInputToken (..),
                                               -- StreamOutputAction (..),
                                               -- StreamWorker,
                                               UnpackedNameValueList (..),
                                               -- getHeader
                                               )
import           Rede.Utils                   (lowercaseText)
import           Rede.Workers.VeryBasic       (bad404ResponseData,
                                               bad404ResponseHeaders)


adaptHeaders :: Int -> UnpackedNameValueList -> UnpackedNameValueList 
adaptHeaders status_code (UnpackedNameValueList raw_headers) = let
    -- Let's remove some headers....
    -- Connection, Host, Keep-Alive, Proxy-Connection, Transfer-Encoding, Accept-Ranges
    no_oniuxus_headers = [ (x,y) | (x,y) <- raw_headers, x `S.notMember` headers_to_remove]

    -- Add 'status' header
    with_status  = (":status", pack $ show status_code):no_oniuxus_headers
    with_version = (":version", "HTTP/1.1"):with_status

    headers_to_send = [ (lowercaseText x, y) | (x,y) <- with_version ]

    headers_to_remove = S.fromList [
        ":status",
        ":version",
        "connection",
        "host",
        "keep-alive",
        "content-encoding",  -- <-- gzip compression goes here
        "date",
        "proxy-connection",
        "transfer-encoding",
        "accept-ranges"] :: S.Set B.ByteString

    -- And be sure to put everything lowercase 
  in 
    UnpackedNameValueList headers_to_send
    

harCoherentWorker :: ResolveCenter -> CoherentWorker 
harCoherentWorker resolve_center (input_headers, _ ) = do 

    -- liftIO $ putStrLn $ "headers: " ++ (show input_headers)
    liftIO $ infoM "HarWorker" $ " .. request to " ++ (show resource_handle)
    let 
        maybe_served_entry  = resolver resource_handle :: Maybe ServedEntry
        resolver = resolveFromHar resolve_center
    -- Not pushing any streams now.... 
    let pushed_streams = []

    case maybe_served_entry of 

        Just served_entry -> let 
                contents = (served_entry ^. sreContents)
                UnpackedNameValueList adapted_headers = adaptHeaders (served_entry ^. sreStatus ) (served_entry ^. sreHeaders)
            in return (adapted_headers , pushed_streams, yield contents)

        Nothing -> do 
            
            return bad404PrincipalStream


  where 
    (Just path)                            = getHeaderFromFlatList input_headers ":path"
    (Just host)                            = getHeaderFromFlatList input_headers ":authority"
    Just (U.URI _ _ u_path u_query u_frag) = U.parseURIReference $ unpack path
    complete_url                           = U.URI {
        U.uriScheme     = "https:"
        ,U.uriAuthority = Just $ U.URIAuth {
            U.uriUserInfo = ""
            ,U.uriRegName = unpack host 
            ,U.uriPort    = ""
            }
        ,U.uriPath      = u_path
        ,U.uriQuery     = u_query 
        ,U.uriFragment  = u_frag 
      }

    -- This string includes the method in front of the schema and everything else...
    resource_handle     = handleFromMethodAndUrl method $ (pack.show) complete_url
    
    (Just method)   = getHeaderFromFlatList input_headers ":method"


bad404PrincipalStream :: PrincipalStream 
bad404PrincipalStream = 
    (
        bad404ResponseHeaders ,
        [],
        yield bad404ResponseData
    )