{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Rede.Utils (
    strToInt
    ,Word24
    ,word24ToInt
    ,putWord24be
    ,getWord24be
    ,getTimeDiff
    ,timeAsDouble 
    ,reportTimedEvent
    ,lowercaseText
    ,unfoldChannelAndSource
    ,stripString
    ,neutralizeUrl
    ,domainFromUrl
    ,hashFromUrl
    ,hashSafeFromUrl
    ,unSafeUrl

    ,SafeUrl
    ) where 


import           Control.Concurrent.Chan
import           Control.Monad.Trans.Class (lift)
import qualified Crypto.Hash.MD5           as MD5
import           Data.Binary               (Binary, get, put, putWord8)
import           Data.Binary.Get           (Get, getWord16be, getWord8)
import           Data.Binary.Put           (Put, putWord16be)
import           Data.Bits
import qualified Data.ByteString           as B
import qualified Data.ByteString.Base16    as B16
import           Data.ByteString.Char8     (pack, unpack)
import           Data.Hashable             (Hashable)
import           Data.Conduit
import qualified Data.Text                 as T
import           Data.Text.Encoding
import qualified Network.URI               as U
import qualified System.Clock              as SC
import           Text.Printf               (printf)
-- import qualified Text.Show.ByteString      as S(Show(..))



strToInt::String -> Int 
strToInt = fromIntegral . toInteger . (read::String->Integer)


newtype Word24 = Word24 Int
    deriving (Show)


-- Newtype to protect url usage
newtype SafeUrl = SafeUrl { unSafeUrl :: B.ByteString } deriving (Eq, Show, Hashable)



word24ToInt :: Word24 -> Int 
word24ToInt (Word24 w24) = w24


instance Binary Word24 where

    put (Word24 w24) = 
        do 
          let 
            high_stuff   = w24 `shiftR` 24 
            low_stuff    = w24 `mod`  (1 `shiftL` 24) 
          putWord8 $ fromIntegral high_stuff
          putWord16be $ fromIntegral low_stuff 

    get = do
      high_stuff <- getWord8 
      low_stuff  <- getWord16be
      let 
        value = (fromIntegral low_stuff) + ( (fromIntegral high_stuff) `shiftL` 24 ) 
      return $ Word24 value


getWord24be :: Get Int
getWord24be = do 
    w24 <- get
    return $ word24ToInt w24


putWord24be :: Int -> Put 
putWord24be x = put (Word24 x)


getTimeDiff :: SC.TimeSpec -> IO Double
getTimeDiff ts0 = do 
    ts1 <- SC.getTime SC.Monotonic
    return $ (timeAsDouble ts1) - (timeAsDouble ts0)


timeAsDouble :: SC.TimeSpec -> Double 
timeAsDouble t = 
    (fromIntegral (SC.sec t)) + (fromIntegral (SC.nsec t))/1.0e9


reportTimedEvent :: SC.TimeSpec -> String -> IO ()
reportTimedEvent base_time message = do
    let 
        base_as_double = timeAsDouble base_time 
    now <- SC.getTime SC.Monotonic
    putStrLn $ "ev: " ++ message ++ " |time/ " ++ (printf "%0.4f seconds" 
        ((timeAsDouble now) - base_as_double) )


lowercaseText :: B.ByteString -> B.ByteString
lowercaseText bs0 = 
    encodeUtf8 ts1 
  where 
    ts1 = T.toLower ts0 
    ts0 = decodeUtf8 bs0


unfoldChannelAndSource :: IO (Chan (Maybe a), Source IO a)
unfoldChannelAndSource = do 
  chan <- newChan 
  let 
    source = do 
      e <- lift $ readChan chan
      case e of 
          Just ee -> do
              yield ee
              source 

          Nothing -> 
              return ()

  return (chan, source)


stripString :: String -> String 
stripString  = filter $ \ ch -> (ch /= '\n') && ( ch /= ' ')


neutralizeUrl :: B.ByteString -> B.ByteString
neutralizeUrl url = let 
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
  in 
    pack $ show complete_url


domainFromUrl :: B.ByteString -> B.ByteString
domainFromUrl url = let 
    Just (U.URI {- scheme -} _ authority _ _ _) = U.parseURI $ unpack url
    Just (U.URIAuth _ use_host _) = authority
  in 
    pack use_host


-- TODO: This constant should not be in the repository
urlHashSalt :: B.ByteString
urlHashSalt = "Adfafwwf"


hashFromUrl :: B.ByteString -> B.ByteString 
hashFromUrl url = 
    B.take 10 . B16.encode . MD5.finalize $ foldl MD5.update MD5.init $  [urlHashSalt, neutralizeUrl url]


hashSafeFromUrl :: B.ByteString -> SafeUrl 
hashSafeFromUrl = SafeUrl . hashFromUrl
