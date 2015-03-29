{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

module Rede.HarFiles.ServedEntry(
    sreStatus 
    ,sreHeaders
    ,sreContents
    ,sreHost
    ,resolveFromHar
    ,createResolveCenter
    ,servedResources
    ,allSeenHosts
    -- ,createResolveCenterFromFilePath
    ,resourceHandleToByteString
    -- ,hostsFromHarFile
    ,handleFromMethodAndUrl
    -- ,createResolveCenterFromLazyByteString
    ,resolveCenterFromLazyByteString
    ,rcName
    ,rcOriginalUrl
    ,handlesAtResolveCenter

    ,ServedEntry  (..)
    ,ResolveCenter
    ,BadHarFile   (..)
    ,ResourceHandle()
    ) where 


import           Control.Exception
import qualified Control.Lens           as L
import           Control.Lens           ( (^.), (&), (.~) )
import           Control.Lens.TH        (makeLenses)
-- import           Data.Char

import           Data.Typeable
import           Data.Maybe             (fromMaybe, isJust, fromJust)
import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import           Network.URI            (parseURI, uriAuthority, uriRegName)
import qualified Network.URI            as U
import qualified Data.ByteString.Lazy   as LB
import           Data.ByteString.Char8  (unpack, pack)
-- import           Data.Text(Text)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
-- import qualified Data.ByteString.Base64 as B64


import           Rede.Utils             (lowercaseText, hashFromUrl)
import           Rede.MainLoop.Tokens   (
                                            UnpackedNameValueList(..)
                                            , getHeader
                                        )


import Rede.HarFiles.JSONDataStructure 


-- How do we identify resources that are being 
-- asked for? Plain ByteString? Wecan start this way...
-- if query-string reordering is an issue, we can supersede
-- this type somehow... 
newtype ResourceHandle = ResourceHandle B.ByteString 
    deriving (Eq, Show, Ord)


resourceHandleToByteString :: ResourceHandle -> B.ByteString
resourceHandleToByteString (ResourceHandle bs) = bs


-- | Individual entries used on resolution
data ServedEntry = ServedEntry {

    -- Status to return to the server
    _sreStatus :: !Int

    -- Headers to return to the server
    ,_sreHeaders :: !UnpackedNameValueList

    -- And other contents which are part of the response,
    -- also to return to the server.
    ,_sreContents :: !B.ByteString

    -- Let's just keep here the host 
    ,_sreHost :: !B.ByteString
    } 


-- | Everything needed to solve things out
data ResolveCenter = ResolveCenter {
    -- Things I'm actually going to serve
    _servedResources ::  M.Map ResourceHandle ServedEntry

    -- A list with all the hosts we have to emulate, without duplicates
    ,_allSeenHosts :: [B.ByteString]

    -- A unique name that can be used to identify this resolve center... 
    ,_rcName :: B.ByteString

    -- The original url 
    ,_rcOriginalUrl :: B.ByteString

    -- Some other results, like the number of resources that 
    -- can't be served because they are in the wrong HTTP method 
    -- or protocol.
    
    }


data BadHarFile = BadHarFile B.ByteString
    deriving (Show, Typeable)

instance Exception BadHarFile


makeLenses ''ServedEntry
makeLenses ''ResolveCenter


-- Here: translate a .HAR file to a lookup function. It may happen 
-- that this function doesn't find the resource, in that case return 
-- Nothing. Another level should decide what response to cook on 
-- that particular scenario. 
--
-- TODO: Entry filtering made down below made need to be reported,
-- and for that, this interface changed.
resolveFromHar :: ResolveCenter -> ResourceHandle -> Maybe ServedEntry
resolveFromHar resolve_center resource_handle = 
    M.lookup resource_handle doc_dic
  where
    doc_dic = _servedResources resolve_center


handlesAtResolveCenter :: ResolveCenter -> [B.ByteString]
handlesAtResolveCenter resolve_center = map 
    (resourceHandleToByteString . fst) 
    (M.toList $ resolve_center ^. servedResources )


-- The function that builds a resolve center. This is the only 
-- place where such resolve centers can be created. Other functions use this one. 
createResolveCenter :: Har_PostResponse -> ResolveCenter
createResolveCenter har_document = 
    ResolveCenter  
        (M.fromList resource_pairs) -- <- Creates a dictionary
        unduplicated_hosts
        hash_piece
        first_url
  where 
    har_log = har_document ^. harLogPR
    resource_pairs = extractPairs har_log
    all_seen_hosts = map (L.view ( L._2 . sreHost) ) resource_pairs 
    unduplicated_hosts = (S.toList . S.fromList) all_seen_hosts
    
    first_url = har_document ^. originUrl 
    hash_piece = hashFromUrl first_url



-- createResolveCenterFromFilePath :: B.ByteString -> IO ResolveCenter
-- createResolveCenterFromFilePath filename = do 
--     file_contents <- LB.readFile $ unpack filename
--     case (decode file_contents :: Maybe Har_Outer ) of 

--         Just doc_model -> return $ createResolveCenter doc_model

--         Nothing -> throw $ BadHarFile  filename


-- createResolveCenterFromLazyByteString :: LB.ByteString -> ResolveCenter
-- createResolveCenterFromLazyByteString file_contents = do 
--     case (decode file_contents :: Maybe Har_Outer ) of 

--         Just doc_model ->  createResolveCenter doc_model

--         Nothing -> throw $ BadHarFile $"InputString"


resolveCenterFromLazyByteString :: LB.ByteString -> ResolveCenter
resolveCenterFromLazyByteString file_contents = do 
    case (eitherDecode file_contents :: Either String Har_PostResponse ) of 

        Right har_post_response ->  
            createResolveCenter har_post_response

        Left msg -> throw $ BadHarFile $ pack msg


-- Convenience function to extract all the hosts from a .har file. 
-- Not very efficient.
-- hostsFromHarFile :: FilePath -> IO [B.ByteString]
-- hostsFromHarFile har_filename = do
--     resolve_center <- createResolveCenterFromFilePath $ pack har_filename
--     return $ resolve_center ^. allSeenHosts


extractPairs :: Har_Log -> [(ResourceHandle, ServedEntry)]
extractPairs har_log = 
    map fromJust $ filter isJust $ map docFromEntry $ filter entryCanBeServed  doc_entries
  where 
    -- Using a lens to fetch the entries from the document. 
    -- The parenthesis are not needed, except as documentation
    doc_entries =har_log ^. entries


-- Right now, we will be filtering out requests which are based on 
-- methods other than GET
entryCanBeServed :: Har_Entry -> Bool 
entryCanBeServed har_entry = http_method == "GET"
  where 
    http_method = har_entry ^. request.method


docFromEntry :: Har_Entry -> Maybe (ResourceHandle, ServedEntry)
docFromEntry e = do
    entry <- servedEntryFromStatusHeadersAndContents
        (resp ^. status)
        (resp ^. respHeaders . L.to harHeadersToUVL)
        content_text
        the_url
    return (
            handleFromMethodAndUrl
                (req ^. method)
                the_url
            , entry 
        )
  where 
    the_url      = (req ^. reqUrl )
    req          = e ^. request
    resp         = e ^. response
    content_text =  fromMaybe "" (resp ^. content . contentText )


-- This not only changes format, it also lower-cases header names. 
-- I do this as a way of normalizing them... 
harHeadersToUVL :: [Har_Header] -> UnpackedNameValueList
harHeadersToUVL h = UnpackedNameValueList $ map 
    (\ har_header ->   ( lowercaseText (har_header ^. headerName ), (har_header ^. headerValue) )
    ) h
    

handleFromMethodAndUrl :: HereString -> HereString -> ResourceHandle
handleFromMethodAndUrl methodx url = 
    ResourceHandle $ methodx `B.append` schema_neuter_url
  where 
    schema_neuter_url = pack $ show complete_url
    Just (U.URI {- scheme -} _ authority u_path u_query u_frag) = U.parseURI $ unpack url
    Just (U.URIAuth _ use_host _) = authority
    complete_url  = U.URI {
        U.uriScheme     = "snu:"
        ,U.uriAuthority = Just $ U.URIAuth {
            U.uriUserInfo = ""
            ,U.uriRegName = use_host 
            ,U.uriPort    = ""
            }
        ,U.uriPath      = u_path
        ,U.uriQuery     = u_query 
        ,U.uriFragment  = u_frag 
      }


servedEntryFromStatusHeadersAndContents :: Int
    -> UnpackedNameValueList 
    -> B.ByteString 
    -> B.ByteString
    -> Maybe ServedEntry
servedEntryFromStatusHeadersAndContents statusx unvl contents the_url = do
    uri <- parseURI $ unpack the_url
    auth <- uriAuthority uri 
    let 
        host_of_url = pack $ uriRegName auth 
        preliminar = ServedEntry statusx unvl contents host_of_url

    return $ heedContentTypeAndDecode preliminar
  -- where 
  --   maybe_uri    = parseURI $ unpack the_url
  --   Just auth   = uriAuthority uri
  --   host_of_url = pack $ uriRegName auth
  --   preliminar  = ServedEntry statusx unvl contents host_of_url


contentsAreBinary :: B.ByteString  -> Bool 
contentsAreBinary content_type = 
    content_type 
    `S.member`
    binary_content_types 
  where 
    binary_content_types = S.fromList [
        "image/png"
        ]


heedContentTypeAndDecode :: ServedEntry -> ServedEntry
heedContentTypeAndDecode served_entry = 

    served_entry & sreContents .~ decoded_contents

  where     

    decoded_contents  =  case maybe_content_type of 

        Just content_type -> 
            if contentsAreBinary content_type 
              then 
                rightly_decoded
              else 
                not_decoded

        Nothing -> not_decoded


    maybe_content_type = getHeader (served_entry ^. sreHeaders) "content-type"
    Right rightly_decoded      = B64.decode not_decoded
    not_decoded = served_entry ^. sreContents