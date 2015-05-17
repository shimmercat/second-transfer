{-# LANGUAGE OverloadedStrings #-}
module SecondTransfer.Utils.HTTPHeaders (
    lowercaseHeaders
    ,headersAreValidHTTP2
    ,fetchHeader
    ,replaceHostByAuthority
    ,headerLens
    ) where 

import qualified Control.Lens                           as L
import           Control.Lens                           ( (^.) )

import qualified Data.ByteString                        as B
import           Data.Char                              (isUpper)
import           Data.List                              (find)
import           Data.Text                              (toLower)
import qualified Data.Text                              as T
import           Data.Text.Encoding                     (decodeUtf8, encodeUtf8)

import           Control.Applicative                    ((<$>))

import           SecondTransfer.MainLoop.CoherentWorker (Headers)

-- Why having a headers library? The one at Network.HTTP.Headers works
-- with Strings and is not very friendly to custom headers. 
-- This is a very basic, lightweight normalizer.

-- | HTTP headers are case-insensitive, so we can use lowercase versions 
-- everywhere
lowercaseHeaders :: Headers -> Headers
lowercaseHeaders = map (\(h,v) -> (low h, v))
  where 
    low = encodeUtf8 . toLower . decodeUtf8


-- | Checks that headers are lowercase
headersAreValidHTTP2 :: Headers -> Bool 
headersAreValidHTTP2 headers = 
  let 
    isOk a_header = not . T.any isUpper . decodeUtf8 . fst $ a_header
  in 
    foldl
        (\ prev e -> (flip (&&)) (isOk  e) $! prev)
        True
        headers

-- | Looks for a given header
fetchHeader :: Headers -> B.ByteString -> Maybe B.ByteString
fetchHeader headers header_name = 
    snd 
      <$> 
    find ( \ x -> fst x == header_name ) headers 


-- | replaceHeaderValue headers header_name maybe_header_value looks for 
--   header_name. If header_name is found and maybe_header_value is nothing, it 
--   returns a new headers list with the header deleted. If header_name is found
--   and header_value is Just new_value, it returns a new list with the header 
--   containing the new value. If header_name is not in headers and maybe_header_value
--   is Nothing, it returns the original headers list. If header_name is not in headers
--   and maybe_header_value is Just new_value, it returns a new list where the last element
--   is (header_name, new_value)
replaceHeaderValue :: Headers -> B.ByteString -> Maybe B.ByteString -> Headers 
replaceHeaderValue headers header_name maybe_header_value = 
    disect id headers
  where 
    disect builder [] = case maybe_header_value of 
        Nothing -> headers
        Just new_value -> builder $ (header_name, new_value):[]
    disect builder ( el@(hn,_) : rest) 
        | hn /= header_name = 
            disect
                (\ constructed_list -> builder $ el:constructed_list )
                rest
        | otherwise = case maybe_header_value of 
            Nothing -> builder rest 
            Just new_value -> builder $ (hn, new_value):rest

headerLens :: 
  Functor f => 
  B.ByteString -> 
  (Maybe B.ByteString -> f (Maybe B.ByteString)) ->
  Headers ->
  f Headers
headerLens header_name f headers | old_header_value <- fetchHeader headers header_name = fmap 
  (\ new_header_value -> replaceHeaderValue headers header_name new_header_value)
  (f old_header_value )

-- | Replaces a "Host" HTTP/1.1 header by an ":authority" HTTP/1.1 
-- header.
-- Notice that having a "Host" header is perfectly valid in certain 
-- circumstances, check 8.1.2.3. However right now I don't see 
-- those circumstances aplying to this library when acting as a pure
-- server.... 
--
-- Anyway, call this function when it makes sense, e.g., when implementing
-- a proxy to an application server. 
replaceHostByAuthority :: Headers -> Headers 
replaceHostByAuthority  headers = 
  let
    host_lens :: L.Lens' Headers (Maybe B.ByteString)
    host_lens = headerLens "host"
    maybe_host_header = headers ^. host_lens
    headers_without_host = L.set host_lens Nothing headers
  in 
    case maybe_host_header of 
        Nothing -> headers 
        Just host -> (":authority", host): headers_without_host