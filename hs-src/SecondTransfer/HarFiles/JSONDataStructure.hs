{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}
module Rede.HarFiles.JSONDataStructure where 


import           Control.Applicative
import           Control.Lens                   (to, (^.))
import           Control.Lens.TH                (makeLenses)
import           Control.Exception
import           Text.Printf                    (printf)

import           Data.Aeson
import           Data.Typeable
import           Data.Aeson.Types               (Parser)
import           Data.ByteString                (ByteString)

import           Data.ByteString.Char8          (pack, unpack)
-- import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as LB

-- import           Data.Functor.Identity

import           Network.URI

-- import           Rede.Utils                     (neutralizeUrl)
import           Rede.Utils.PrintfArgByteString ()

-- import           Debug.Trace                    (trace)



-- Implementation details ####################################


data HarStructureLogicalError = HarStructureLogicalError String 
    deriving (Show, Typeable)


instance Exception HarStructureLogicalError 



-- String type to be used when de-serializing an 
-- element from JSON har
type HereString = ByteString


data Har_Log = Har_Log {
    _entries           :: ![Har_Entry]
    ,_pages            :: ![Har_Page]
    -- ,_browser          :: !Har_VersionPair
    ,_version          :: !HereString 
    ,_creator          :: !Har_VersionPair
   }


data Har_Header = Har_Header {
    _headerName   :: !HereString
    ,_headerValue :: !HereString 
    }


data Har_Response = Har_Response {
    _status       :: !Int 
    ,_content     :: !Har_Content
    ,_respHeaders :: ![Har_Header]
    }


-- There are more fields hidden in this...
data Har_Content   = Har_Content {
    _contentText  :: ! (Maybe HereString)
    }


data Har_Request = Har_Request {
    _method       :: !HereString
    ,_reqHeaders  :: ![Har_Header]
    ,_queryString :: ![Har_QueryString]
    ,_reqUrl      :: !HereString
    ,_reqBody     :: !HereString
    }


data Har_Outer = Har_Outer {
    _harLog:: !Har_Log
    }


data Har_PostResponse = Har_PostResponse {
    _harLogPR   :: !Har_Log
    ,
    _originUrl :: !HereString
    }


data Har_Page = Har_Page {
    _startedDateTime   :: !HereString 
    ,_pageTimings      :: !Har_PageTimings
    ,_pageId           :: !HereString
    ,_title            :: !HereString
   }


data Har_QueryString = Har_QueryString {
   _qsName               ::  !HereString 
   ,_qsValue             ::  !HereString
   }


data Har_PageTimings = Har_PageTimings {
    -- Milliseconds
    _onContentLoad     :: !Int 
    --,_comment          :: !HereString 
    -- Milliseconds
    ,_onLoad           :: !Int 
   }


data Har_VersionPair = Har_VersionPair {
   _vpVersion          :: !HereString 
   ,_vpName            :: !HereString
   }


-- I'm only taking some of the fields on this object....
data Har_Entry = Har_Entry {
    _request           :: !Har_Request
    ,_response         :: !Har_Response
   }


makeLenses ''Har_Page
makeLenses ''Har_Outer
makeLenses ''Har_Response
makeLenses ''Har_Entry 
makeLenses ''Har_Header
makeLenses ''Har_VersionPair
makeLenses ''Har_Log
makeLenses ''Har_Request
makeLenses ''Har_PageTimings
makeLenses ''Har_QueryString
makeLenses ''Har_Content
makeLenses ''Har_PostResponse





-- JSON parsing --------------------------------------------------------------------


errorParse  :: Value -> Parser a
errorParse  x = error $ printf "NotGoodHere: %s" $ show x


instance FromJSON Har_VersionPair where 

    parseJSON (Object v)  = Har_VersionPair <$>
        ( pack <$>  v .: "version" )    <*>
        ( pack <$>  v .: "name"    )

    parseJSON  x =  errorParse x


instance FromJSON Har_PageTimings where 

    parseJSON (Object v)  = Har_PageTimings <$> 
         (v .:? "onContentLoad"  .!= (-1) )  <*>
         (v .:? "onLoad" .!= (-1) )


instance FromJSON Har_Header where 

    parseJSON (Object v) = Har_Header <$> 
        (pack <$> v .: "name"  )       <*>
        (pack <$> v .: "value" )        

    parseJSON  x         = errorParse x


instance FromJSON Har_Response where 

    parseJSON (Object v) = Har_Response <$>
        v .: "status"                  <*>
        v .: "content"                 <*>
        v .: "headers"


instance FromJSON Har_Request where 

    parseJSON (Object v) = Har_Request <$> 
        (pack <$> v .: "method" )      <*>
        v .: "headers"                 <*>
        v .: "queryString"             <*>
        (pack <$> v .: "url" )         <*>
        (pure "")


instance FromJSON Har_Log where 

    parseJSON (Object v) = Har_Log  <$> 
        v .: "entries"              <*>
        v .: "pages"                <*>
        -- v .: "browser"              <*>
        (pack <$> v .: "version")   <*>
        v .: "creator"


instance FromJSON Har_Page where 

    parseJSON (Object v) = Har_Page  <$>
        (pack <$> v .: "startedDateTime") <*>
        v .: "pageTimings" <*>
        (pack <$> v .: "id") <*>
        (pack <$> v .: "title")


instance FromJSON  Har_Entry where 

    parseJSON (Object v) = Har_Entry  <$>
        v .: "request"   <*>
        v .: "response"


instance FromJSON Har_QueryString where 

    parseJSON (Object v) = Har_QueryString <$>
        (pack <$> v .: "name" )     <*>
        (pack <$> v .: "value")


instance FromJSON Har_Content where 
    parseJSON (Object v) = Har_Content <$>
        ( (liftA pack) <$> v .:? "text" )


instance FromJSON Har_Outer where 

    parseJSON (Object v) = Har_Outer <$> 
        v .: "log"


instance FromJSON  Har_PostResponse where 

    parseJSON (Object v) = Har_PostResponse <$>
        v .:  "har"                      <*>
        (pack <$> v .: "originUrl" )    



-- Get the first url out from a Har Log 
firstUrlFromHarLog :: Har_Log -> HereString
firstUrlFromHarLog lg = let 
    es = head $ lg ^. entries 
    url = es ^. request . reqUrl
  in url
        

firstDomainFromHarLog :: Har_Log -> HereString
firstDomainFromHarLog lg = let 
    full_url = firstUrlFromHarLog lg 
    Just parsed_url = parseURI . unpack $ full_url
    maybe_domain = do  -- Maybe monad
        auth <- uriAuthority $ parsed_url
        return $ uriRegName auth
  in case maybe_domain of 
    Just domain -> pack domain
    Nothing -> throw (
        HarStructureLogicalError $ printf "First domain failed to parse, it was: %s" full_url)


------ Some small functions to try this out 
test_01 :: IO ()
test_01 = do 
    file_contents <- LB.readFile "tests/hackage.haskell.org3.har"
    case (eitherDecode file_contents :: Either String Har_Outer ) of 
    
        Right doc_model -> do 
            putStrLn $ printf "File has %d entries" $ doc_model ^. harLog . entries . to length

        Left  msg       -> do 
            putStrLn $ printf "Apparently parsing failed: %s" msg


test_02 :: IO Har_Outer
test_02 = do 
    -- Can we hook with urls?
    file_contents <- LB.readFile "tests/bitbucket.org.har"
    case (decode file_contents :: Maybe Har_Outer ) of 

        Just doc_model -> return doc_model

        Nothing -> error "Just got an error"
