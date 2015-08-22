{-# LANGUAGE OverloadedStrings #-}
import SecondTransfer(
    tlsServeWithALPN
    , http2Attendant
    , http11Attendant
    , dropIncomingData
    )
import SecondTransfer.Sessions(
      makeSessionsContext
    , defaultSessionsConfig
    )

import           System.Mem (performGC)

import qualified Data.ByteString as B
import           Data.ByteString.Char8 (pack)
import           Data.Conduit


import SecondTransfer.Utils.HTTPHeaders
import SecondTransfer.Types

usedAtSite :: B.ByteString
usedAtSite = "www.httpdos.com:9043"

htmlFile :: B.ByteString
htmlFile = "<!DOCTYPE html>\n \
   \ <html><head><title>Hello</title><link rel=\"stylesheet\" type=\"text/css\" \
   \ href=\"/mycss.css\"> </head><body>Hello</body></html> \
   \ "

css1 :: B.ByteString
css1 = "html {background-color: red; }"

css2 :: B.ByteString
css2 = "html {background-color: green; }"

css3 :: B.ByteString
css3 = "html {background-color: orange; }"

css4 :: B.ByteString
css4 = "html {background-color: yellow; }"


says :: B.ByteString -> DataAndConclusion
says msg = do
    -- The data in each yield will be automatically split across multiple
    -- data frames if needed, so you can yield a large block of contents here
    -- if you wish.
    -- If you do multiple yields, no data will be left buffered between them,
    -- so that you can for example implement a chat client in a single HTTP/2 stream.
    -- Not that browsers support that.
    yield msg
    -- The HTTP/2 protocol supports sending headers *after* stream data. So, you usually
    -- return an empty list to signal that you don't want any of those headers.
    -- In these docs, the post headers are often called "footers".
    return []


lengthHeader :: B.ByteString -> (B.ByteString, B.ByteString)
lengthHeader s = ("content-length", pack . show $ B.length s)


-- Let's throw some performance away
directlyIntroduceDate :: Headers -> IO Headers
directlyIntroduceDate =  fmap toList . introduceDateHeader . fromList


simpleWorker :: AwareWorker
simpleWorker = coherentToAwareWorker $ \ (request_headers, maybe_post_data) -> do
    let
        Just requested_path = fetchHeader request_headers ":path"
        maybe_condition = fetchHeader request_headers "if-none-match"
    dropIncomingData maybe_post_data
    result <- case requested_path of
        "/"    | Nothing <- maybe_condition -> do
                   response_headers  <- directlyIntroduceDate [
                           (":status", "200"),
                           ("content-type", "text/html"),
                           ("cache-control", "max-age=1"),
                           ("etag", "version1"),
                           lengthHeader htmlFile
                       ]
                   return (
                       response_headers,
                       [], -- No pushed streams
                       says htmlFile
                       )
               | Just "version1" <- maybe_condition ->  do
                   response_headers  <- directlyIntroduceDate [
                           (":status", "200"),
                           ("etag", "version2"),
                           lengthHeader htmlFile
                       ]
                   return (
                       response_headers,
                       [
                       ],
                       says htmlFile
                       )
               | otherwise -> do
                   response_headers  <- directlyIntroduceDate [
                           (":status", "200"),
                           ("etag", "version3"),
                           ("server-did", "will-push"),
                           lengthHeader htmlFile
                       ]
                   return (
                       response_headers,
                       [
                         -- Push some invalidation header for the css
                         do
                             pushed_header <-  directlyIntroduceDate [
                                  (":status", "200"),
                                  ("content-type", "text/css"),
                                  ("etag", "versionr2"),
                                  ("cache-control", "max-age=100000"),
                                  lengthHeader css2
                                 ]
                             return PushedStream {
                                 _requestHeaders_Psh = [
                                       (":authority", usedAtSite ),
                                       (":method", "GET"),
                                       (":scheme", "https"),
                                       (":path", "/mycss.css"),
                                       ("if-none-match", "version1"),
                                       ("server-did", "pushed"),
                                       ("content-type", "text/css")
                                 ],
                                 _responseHeaders_Psh = pushed_header,
                                 _dataAndConclusion_Psh = says css2
                             }
                       ],
                       says htmlFile
                       )
        "/mycss.css" | Nothing <- maybe_condition -> do
                         response_headers <- directlyIntroduceDate  [
                                        (":status", "200"),
                                        ("etag", "version1"),
                                        ("content-type", "text/css"),
                                        ("cache-control", "max-age=100000"),
                                        lengthHeader css1
                                    ]
                         return (
                            response_headers,
                            [],
                            says css1
                            )

        -- The branch below will be taken if the browser tries to revalidate this resource, there
        -- an orange page will appear.
                     | Just "version1" <- maybe_condition -> do
                         response_headers <- directlyIntroduceDate  [
                                        (":status", "200"),
                                        ("etag", "version5"),
                                        ("content-type", "text/css"),
                                        ("cache-control", "max-age=100000"),
                                        lengthHeader css3
                                    ]
                         return (
                            response_headers,
                            [],
                            says css3
                            )

         -- When trying to revalidate yet another thing, go yellow
                     | otherwise -> do
                         response_headers <- directlyIntroduceDate  [
                                        (":status", "200"),
                                        ("etag", "version6"),
                                        ("content-type", "text/css"),
                                        ("cache-control", "max-age=100000"),
                                        lengthHeader css4
                                    ]
                         return (
                            response_headers,
                            [],
                            says css4
                            )
        _  -> do
            response_headers <- directlyIntroduceDate  [
                           (":status", "404"),
                           ("content-type", "text/plain"),
                           lengthHeader "not found"
                       ]
            return (
               response_headers,
               [],
               says "not found"
               )
    performGC
    return result



-- For this program to work, it should be run from the top of
-- the developement directory...because of the certificates below.
-- But you are welcome to use your own.
main :: IO ()
main = do
    sessions_context <- makeSessionsContext defaultSessionsConfig
    let
        http2_attendant = {-# SCC a2 #-} http2Attendant sessions_context   simpleWorker
        http11_attendant = http11Attendant sessions_context simpleWorker
    do {-# SCC serveStuff #-} tlsServeWithALPN
        "tests/support/servercert.pem"   -- Server certificate
        "tests/support/privkey.pem"      -- Certificate private key
        "127.0.0.1"                      -- On which interface to bind
        [
            ("no-protocol", http11_attendant), -- The first protocol in the list is used when
                                               -- when no ALPN negotiation happens, and the
                                               -- name is really a filler.
            ("h2-14", http2_attendant),    -- Protocols present in the ALPN negotiation
            ("h2",    http2_attendant),    -- they may be slightly different, but for this
                                           -- test it doesn't matter.

            ("http1.1", http11_attendant) -- Let's talk HTTP1.1 if everything else fails.
        ]
        9043
