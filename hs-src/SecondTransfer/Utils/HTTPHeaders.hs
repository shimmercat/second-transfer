{-# LANGUAGE OverloadedStrings, Rank2Types #-}
{-|

Utilities for working with headers. 

-}
module SecondTransfer.Utils.HTTPHeaders (
    -- * Simple manipulation
    -- 
    -- | These transformations are simple enough that don't require
    --   going away from the list representation (see type `Headers`) 
    lowercaseHeaders
    ,headersAreValidHTTP2
    ,fetchHeader
    -- * Transformations based on maps
    --
    -- | Many operations benefit
    --   from transforming the list to a map with custom sorting and 
    --   doing a set of operations on that representation.
    --
    ,HeaderEditor
    -- ** Introducing and removing the `HeaderEditor`
    ,fromList 
    ,toList 
    -- ** Access to a particular header
    ,headerLens
    ,replaceHeaderValue
    -- ** HTTP utilities
    ,replaceHostByAuthority
    ) where 

import qualified Control.Lens                           as L
import           Control.Lens                           ( (^.) )

import qualified Data.ByteString                        as B
import           Data.Char                              (isUpper)
import           Data.List                              (find)
import           Data.Text                              (toLower)
import qualified Data.Text                              as T
import           Data.Text.Encoding                     (decodeUtf8, encodeUtf8)
import qualified Data.Map.Strict                        as Ms
import           Data.Word                              (Word8)

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


newtype Autosorted = Autosorted { toFlatBs :: B.ByteString }
  deriving Eq


colon :: Word8 
colon = fromIntegral . fromEnum $ ':'


instance Ord Autosorted where 
  compare (Autosorted a) (Autosorted b) | (B.head a) == colon, (B.head b) /= colon = LT 
  compare (Autosorted a) (Autosorted b) | (B.head a) /= colon, (B.head b) == colon = GT 
  compare (Autosorted a) (Autosorted b)  = compare a b


-- | Abstract data-type. Use `fromList` to get one of these from `Headers`. 
-- The underlying representation admits better asymptotics.
newtype HeaderEditor = HeaderEditor { innerMap :: Ms.Map Autosorted B.ByteString }


-- | /O(n*log n)/ Builds the editor from a list. 
fromList :: Headers -> HeaderEditor 
fromList = HeaderEditor . Ms.fromList . map (\(hn, hv) -> (Autosorted hn, hv))

-- | /O(n)/ Takes the HeaderEditor back to Headers
toList :: HeaderEditor -> Headers 
toList (HeaderEditor m) = [ (toFlatBs x, v) | (x,v) <- Ms.toList m ]


-- | replaceHeaderValue headers header_name maybe_header_value looks for 
--   header_name. If header_name is found and maybe_header_value is nothing, it 
--   returns a new headers list with the header deleted. If header_name is found
--   and header_value is Just new_value, it returns a new list with the header 
--   containing the new value. If header_name is not in headers and maybe_header_value
--   is Nothing, it returns the original headers list. If header_name is not in headers
--   and maybe_header_value is Just new_value, it returns a new list where the last element
--   is (header_name, new_value)
replaceHeaderValue :: HeaderEditor -> B.ByteString -> Maybe B.ByteString -> HeaderEditor
replaceHeaderValue (HeaderEditor m) header_name maybe_header_value = 
    HeaderEditor $ Ms.alter (const maybe_header_value) (Autosorted header_name) m


-- | headerLens header_name represents a lens into the headers,
--   and you can use it then to add, alter and remove headers.
--   It uses the same semantics than `replaceHeaderValue`
headerLens :: B.ByteString -> L.Lens' HeaderEditor (Maybe B.ByteString)
headerLens name = 
    L.lens 
        (Ms.lookup hname . innerMap ) 
        (\(HeaderEditor hs) mhv -> HeaderEditor $ Ms.alter (const mhv) hname hs)
  where 
    hname = Autosorted name

-- | Replaces a \"host\" HTTP\/1.1 header by an ":authority" HTTP\/2 
-- header.
-- The list is expected to be already in lowercase, so nothing will happen if there
-- the header name portion is \"Host\" instead of \"host\".
--
-- Notice that having a "Host" header in an HTTP\/2 message is perfectly valid in certain 
-- circumstances, check <https://http2.github.io/http2-spec/#rfc.section.8.1.2.3 Section 8.1.2.3>
-- of the spec for details.
replaceHostByAuthority :: HeaderEditor -> HeaderEditor
replaceHostByAuthority  headers = 
  let
    host_lens :: L.Lens' HeaderEditor (Maybe B.ByteString)
    host_lens = headerLens "host"
    authority_lens = headerLens ":authority"
    maybe_host_header = headers ^. host_lens
    no_hosts = L.set host_lens Nothing headers
  in 
    case maybe_host_header of 
        Nothing -> headers 
        Just host -> L.set authority_lens (Just host) no_hosts