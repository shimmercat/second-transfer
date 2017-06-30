{-# LANGUAGE OverloadedStrings, Rank2Types, GeneralizedNewtypeDeriving, DeriveGeneric, TemplateHaskell  #-}
{-|

Utilities for working with headers.

-}
module SecondTransfer.Utils.HTTPHeaders (
    -- * Simple manipulation
    --
    -- | These transformations are simple enough that don't require
    --   going away from the list representation (see type `Headers`)
    lowercaseHeaders
    , headersAreLowercase
    , sanitizeHeaderValue


    -- ** HTTP utilities
    , replaceHostByAuthority
    , introduceDateHeader
    , introduceDateHeaderModifier

    , combineAuthorityAndHost
    , removeConnectionHeaders
    , promoteHost

    , setIfNeeded

    , PrettyPrintHeadersConfig        (..)
    , indentSpace_PPHC
    , prettyPrintHeaders
    , defaultPrettyPrintHeadersConfig

    , headerIsPseudo

    ) where

import qualified Control.Lens                           as L
import           Control.Lens                           ( (^.) )

import qualified Data.ByteString                        as B
import qualified Data.ByteString.Char8                  as B8
import qualified Data.ByteString.Lazy                   as LB
import qualified Data.ByteString.Builder                as Bu
import           Data.ByteString.Char8                  (pack)
import           Data.Char                              (isUpper, toLower)

import qualified Data.Map.Strict                        as Ms

import           Data.Time.Clock                        (getCurrentTime)


import           SimpleHttpHeadersHq


-- Why having a headers library? The one at Network.HTTP.Headers works
-- with Strings and is not very friendly to custom headers.
-- This is a very basic, lightweight normalizer.

-- | HTTP headers are case-insensitive, so we can use lowercase versions
-- everywhere. This function works on the tuple, raw form of headers before
-- they are assimilated by HqHeaders
lowercaseHeaders :: [(B.ByteString, B.ByteString)] ->  [(B.ByteString, B.ByteString)]
lowercaseHeaders = map (\(h,v) -> (lower h, v))


lower :: B.ByteString -> B.ByteString
lower = B8.map toLower


-- | Checks that headers are lowercase
headersAreLowercase :: [(B.ByteString, B.ByteString)] -> Bool
headersAreLowercase headers =
    foldl
        (\ prev (hn, _) -> (flip (&&)) (aTitleIsLowercase  hn) $! prev)
        True
        headers


aTitleIsLowercase :: B.ByteString -> Bool
aTitleIsLowercase a_title = not $ B8.any isUpper $ a_title


-- | Transforms a header value so that characters outside the valid ascii range
--   are replaced by nothings
sanitizeHeaderValue :: B.ByteString -> B.ByteString
sanitizeHeaderValue hv = B.filter (\x -> x >= 32 && x <= 126) hv


-- | Sets only a header that is not set
setIfNeeded ::  HqHeaders -> L.Lens' HqHeaders (Maybe a) ->  a -> HqHeaders
setIfNeeded headers header_access header_value =
    L.over header_access
       (\ old_value -> case old_value of
               Nothing -> Just header_value
               _       -> old_value )
       headers


-- | Replaces a \"host\" HTTP\/1.1 header by an ":authority" HTTP\/2
-- header.
-- The list is expected to be already in lowercase, so nothing will happen if there
-- the header name portion is \"Host\" instead of \"host\".
--
-- Notice that having a "Host" header in an HTTP\/2 message is perfectly valid in certain
-- circumstances, check <https://http2.github.io/http2-spec/#rfc.section.8.1.2.3 Section 8.1.2.3>
-- of the spec for details.
replaceHostByAuthority :: HqHeaders -> HqHeaders
replaceHostByAuthority  headers =
  let
    maybe_host_header = headers ^. host_Hi
    no_hosts = L.set host_Hi Nothing headers
  in
    case maybe_host_header of
        Nothing -> headers
        h@(Just _) -> L.set authority_Hi h no_hosts


-- | Converts a 'host' header to an ':authority' header when there
--   is no authority header
promoteHost :: HqHeaders -> HqHeaders
promoteHost editor =
  let
      maybe_authority = editor ^. authority_Hi
  in case maybe_authority of
      Nothing -> L.set authority_Hi (editor ^. host_Hi) editor
      Just _ -> editor



-- | Combines ":authority" and "host", giving priority to the first. This is used when proxying
--   HTTP/2 to HTTP/1.1. It leaves whichever header of highest priority is present
combineAuthorityAndHost :: HqHeaders -> HqHeaders
combineAuthorityAndHost headers =
  let
    maybe_host_header = headers ^. host_Hi
    maybe_auth_header = headers ^. authority_Hi
    no_hosts = L.set host_Hi Nothing headers
    in case (maybe_auth_header, maybe_host_header) of
        (Just _, _) -> headers
        (Nothing, h@(Just _)) -> L.set authority_Hi h no_hosts
        _ -> headers


-- | Given a header editor, introduces a "Date" header. This function has
-- a side-effect: to get the current time
introduceDateHeader :: HqHeaders -> IO HqHeaders
introduceDateHeader headers = do
    current_time <- getCurrentTime
    return $ L.set date_Hi (Just current_time) headers


-- | As introduceDateHeader, but returns a modifier instead
introduceDateHeaderModifier :: IO (HqHeaders ->  HqHeaders)
introduceDateHeaderModifier  = do
    current_time <- getCurrentTime
    return $ L.set date_Hi (Just current_time)


-- | Remove connection-specific headers as required by HTTP/2
removeConnectionHeaders :: HqHeaders -> HqHeaders
removeConnectionHeaders  =
    L.over otherHeaders_Hi (\ headers ->
    Ms.filterWithKey (\ h _v ->
               (h /= "keep-alive") &&
               (h /= "proxy-connection") &&
               (h /= "transfer-encoding") &&
               (h /= "upgrade")
           ) headers )



-- Support for pretty printing headers -----------------------------------------

data PrettyPrintHeadersConfig = PrettyPrintHeadersConfig {
    _indentSpace_PPHC       :: Int
    }

L.makeLenses ''PrettyPrintHeadersConfig


defaultPrettyPrintHeadersConfig :: PrettyPrintHeadersConfig
defaultPrettyPrintHeadersConfig = PrettyPrintHeadersConfig {
    _indentSpace_PPHC       = 8
    }

prettyPrintHeaders :: PrettyPrintHeadersConfig -> HqHeaders -> B.ByteString
prettyPrintHeaders config hqheaders =
    LB.toStrict . Bu.toLazyByteString . mconcat $ map (\ (h,v) ->
                    space `mappend` Bu.byteString h `mappend` ": " `mappend` Bu.byteString v `mappend` "\n"
                )
                headers
  where
     space = Bu.byteString . pack . take (config ^. indentSpace_PPHC) . repeat $ ' '
     headers = hqheaders ^.  serialized_HqH

headerIsPseudo :: B.ByteString -> Bool
headerIsPseudo x = B8.head x == ':'
