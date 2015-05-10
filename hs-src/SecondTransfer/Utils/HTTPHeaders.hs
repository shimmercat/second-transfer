module SecondTransfer.Utils.HTTPHeaders (
    lowercaseHeaders
    ,headersAreValidHTTP2
    ) where 

import           Data.Char          (isUpper)
import           Data.Text          (toLower)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)


import SecondTransfer.MainLoop.CoherentWorker      (Headers)

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