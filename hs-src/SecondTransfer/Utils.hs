{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.Utils (
    strToInt
    ,Word24
    ,word24ToInt
    ,putWord24be
    ,getWord24be
    ,lowercaseText
    ,unfoldChannelAndSource
    ,stripString
    ,domainFromUrl
    ,subByteString
    ) where


import           Control.Concurrent.MVar
import           Control.Monad.Trans.Class (lift)
import           Data.Binary               (Binary, get, put, putWord8)
import           Data.Binary.Get           (Get, getWord16be, getWord8)
import           Data.Binary.Put           (Put, putWord16be)
import           Data.Bits
import qualified Data.ByteString           as B
import           Data.ByteString.Char8     (pack, unpack)
import           Data.Conduit
import qualified Data.Text                 as T
import           Data.Text.Encoding
import qualified Network.URI               as U


strToInt::String -> Int
strToInt = fromIntegral . toInteger . (read::String->Integer)


newtype Word24 = Word24 Int
    deriving (Show)


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


lowercaseText :: B.ByteString -> B.ByteString
lowercaseText bs0 =
    encodeUtf8 ts1
  where
    ts1 = T.toLower ts0
    ts0 = decodeUtf8 bs0


unfoldChannelAndSource :: IO (MVar (Maybe a), Source IO a)
unfoldChannelAndSource = do
  chan <- newEmptyMVar
  let
    source = do
      e <- lift $ takeMVar chan
      case e of
          Just ee -> do
              yield ee
              source

          Nothing ->
              return ()

  return (chan, source)


stripString :: String -> String
stripString  = filter $ \ ch -> (ch /= '\n') && ( ch /= ' ')


domainFromUrl :: B.ByteString -> B.ByteString
domainFromUrl url = let
    Just (U.URI {- scheme -} _ authority _ _ _) = U.parseURI $ unpack url
    Just (U.URIAuth _ use_host _) = authority
  in
    pack use_host


-- Returns the sub-bytestring that starts at start_pos and ends
-- just before end_pos
subByteString :: Int -> Int -> B.ByteString -> B.ByteString
subByteString start_pos end_pos  =
    B.take (end_pos - start_pos ) . B.drop start_pos
