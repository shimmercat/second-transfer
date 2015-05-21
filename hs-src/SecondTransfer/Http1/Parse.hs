{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module SecondTransfer.Http1.Parse(
    newIncrementalHttp1Parser
    ,addBytes

    -- Internal exports, used by the test suite
    ,locateCRLFs
    ,splitByColon
    ,stripBs
    ,headerListToHTTP11Text
    ,serializeHTTPResponse

    ,IncrementalHttp1Parser
    ,Http1ParserCompletion(..)
    ,BodyStopCondition(..)
    ) where 



import           Control.Exception                      (throw)
-- import           Control.Lens
import qualified Control.Lens                           as L
import           Control.Applicative

import qualified Data.ByteString                        as B
import           Data.List                              (foldl')
import qualified Data.ByteString.Builder                as Bu
import           Data.ByteString.Char8                  (pack, unpack)
import qualified Data.ByteString.Char8                  as Ch8
import qualified Data.ByteString.Lazy                   as Lb
import           Data.Char                              (toLower)
import           Data.Maybe                             (isJust)
import           Data.Monoid                            (mappend,
                                                         mempty,
                                                         mconcat)
import qualified Data.Attoparsec.ByteString             as Ap

import           Data.Foldable                          (find)
import           Data.Word                              (Word8)
import qualified Data.Map                               as M

import qualified SecondTransfer.Utils.HTTPHeaders       as E
import           SecondTransfer.Exception
import           SecondTransfer.MainLoop.CoherentWorker (Headers)
import           SecondTransfer.Utils                   (subByteString)




data IncrementalHttp1Parser = IncrementalHttp1Parser {
    _fullText :: Bu.Builder
    ,_stateParser :: HeaderParseClosure
    }

type HeaderParseClosure = (B.ByteString ->  ([Int], Int, Word8))

-- L.makeLenses ''IncrementalHttp1Parser

instance Show IncrementalHttp1Parser where 
    show (IncrementalHttp1Parser ft _sp ) = show $ Bu.toLazyByteString ft


newIncrementalHttp1Parser :: IncrementalHttp1Parser
newIncrementalHttp1Parser = IncrementalHttp1Parser {
    _fullText = mempty
    ,_stateParser = locateCRLFs 0 [] 0
    }


-- | Was the parser complete?
data Http1ParserCompletion = 
    -- | No, not even headers are done. Use the returned
    --   value to continue
    MustContinue_H1PC IncrementalHttp1Parser
    -- | Headers were completed. For some HTTP methods that's all
    --   there is, and that's what this case represents. The second
    --   argument is a left-overs string.
    |OnlyHeaders_H1PC      Headers B.ByteString
    -- | For requests with a body. The second argument is a condition
    --   to stop receiving the body, the third is leftovers from 
    --   parsing the headers.
    |HeadersAndBody_H1PC   Headers BodyStopCondition B.ByteString
    -- | Some requests are mal-formed. We can check those cases 
    --   here.
    |RequestIsMalformed_H1PC 
    deriving Show


-- | Stop condition when parsing the body. Right now only length 
--   is supported, given with Content-Length. 
--
--  TODO: Support "chunked" transfer encoding for classical 
--  HTTP/1.1, uploads will need it.
data BodyStopCondition = 
     UseBodyLength_BSC Int 
     deriving (Show, Eq)


data RequestOrResponseLine = 
    -- First argument is the URI, second the method
    Request_RoRL B.ByteString B.ByteString
    -- First argument is the status code
    |Response_RoRL Int
    deriving (Show, Eq)

    
addBytes :: IncrementalHttp1Parser -> B.ByteString -> Http1ParserCompletion
addBytes (IncrementalHttp1Parser full_text header_parse_closure) new_bytes =
  let -- Just feed the bytes
    (positions, length_so_far, last_char ) = header_parse_closure new_bytes
    new_full_text = full_text `mappend` (Bu.byteString new_bytes)
    could_finish = twoCRLFsAreConsecutive positions 
  in 
    case could_finish of 
        Just at_position -> elaborateHeaders new_full_text positions at_position

        Nothing -> MustContinue_H1PC 
                    $ IncrementalHttp1Parser 
                        new_full_text 
                        (locateCRLFs length_so_far positions last_char)


elaborateHeaders :: Bu.Builder -> [Int] -> Int -> Http1ParserCompletion
elaborateHeaders full_text crlf_positions last_headers_position = 
  let 
    -- Start by getting a full byte-string representation of the headers,
    -- no need to be silly with chunks.
    full_headers_text = Lb.toStrict $ Bu.toLazyByteString full_text

    -- Filter out CRLF pairs corresponding to multiline headers.
    no_cont_positions_reverse = filter 
        (\ pos -> if pos >= last_headers_position then True else 
            not . isWsCh8 $
                (Ch8.index 
                    full_headers_text
                    (pos + 2)
                )
        )
        crlf_positions

    no_cont_positions = reverse . tail $ no_cont_positions_reverse

    -- Now get the headers as slices from the original string.
    headers_pre = map 
        (\ (start, stop) -> 
            subByteString start stop full_headers_text
        )
        (zip
            ((:)
                0
                (map 
                    ( + 2 )
                    no_cont_positions
                )
            )
            no_cont_positions
        )

    -- TODO: We must reject header names ending in space.
    headers_0 = map splitByColon $ tail headers_pre

    -- The first line is not actually a header, but contains the method, the version
    -- and the URI
    request_or_response = parseFirstLine (head headers_pre)

    headers_1 = headers_0

    (headers_2, has_body) = case request_or_response of 

        Request_RoRL uri method -> 
          let
            lc_method = bsToLower method
            has_body' = case lc_method of 
                "post"   -> True 
                "put"    -> True 
                _        -> False
          in 
            ( (":path", uri):(":method",lc_method):headers_1, has_body' )

        Response_RoRL status -> 
          let 
            status_str = pack . show $ status 
            excludes_body = 
                ( (Ch8.head status_str) == '1')
                ||
                ( status == 204 || status == 304)
          in
            ((":status", status_str): headers_1, not excludes_body)

    -- Still we need to lower-case header names, and trim them
    headers_3 = [
        ( (stripBs . bsToLower $ hn), stripBs hv ) | (hn, hv) <- headers_2
        ]

    content_length :: Int 
    content_length = 
      let
        cnt_length_header = find (\ x -> (fst x) == "content-length" ) headers_3
      in case cnt_length_header of 
        Just (_, hv) -> read . unpack $ hv 
        Nothing -> throw ContentLengthMissingException

    leftovers = B.drop (last_headers_position + 4) full_headers_text
  in 
    if has_body 
      then 
        HeadersAndBody_H1PC headers_3 (UseBodyLength_BSC content_length) leftovers
      else 
        OnlyHeaders_H1PC headers_3 leftovers


splitByColon :: B.ByteString -> (B.ByteString, B.ByteString)
splitByColon  = L.over L._2 (B.tail) . Ch8.break (== ':') 


parseFirstLine :: B.ByteString -> RequestOrResponseLine
parseFirstLine s = 
  let 
    either_error_or_rrl = Ap.parseOnly httpFirstLine s 
    exc = HTTP11SyntaxException "BadMessageFirstLine"
  in 
    case either_error_or_rrl of 
        Left _ -> throw exc 
        Right rrl -> rrl

bsToLower :: B.ByteString -> B.ByteString
bsToLower = Ch8.map toLower 


-- This ought to be slow!
stripBs :: B.ByteString -> B.ByteString
stripBs s = 
    fst 
    .
    last
    $
    takeWhile 
        ( \ (_, ch) -> isWsCh8 ch )
    $
        iterate 
        ( \ (bs, _) -> 
                case Ch8.unsnoc bs of 
                    Just (newbs, w8) -> (newbs, w8)
                    Nothing -> ("", 'n')
        )
        (Ch8.dropWhile isWsCh8 s, ' ')


locateCRLFs :: Int -> [Int] -> Word8 ->  B.ByteString ->  ([Int], Int, Word8)
locateCRLFs initial_offset other_positions prev_last_char next_chunk =
  let 
    (last_char, positions_list, strlen) =
        B.foldl 
            (\ (prev_char, lst, i) w8 -> 
                let 
                    j = i + 1
                in case (prev_char, w8) of 
                    (13,10) -> (w8, (i-1):lst, j)
                    _       -> (w8, lst,   j)
            )
            (prev_last_char, other_positions, initial_offset)
            next_chunk
  in (positions_list, strlen, last_char)


twoCRLFsAreConsecutive :: [Int] -> Maybe Int 
twoCRLFsAreConsecutive (p2:p1:_) | p2 - p1 == 2 = Just p1
twoCRLFsAreConsecutive _                        = Nothing


isWsCh8 :: Char -> Bool
isWsCh8 s = isJust (Ch8.elemIndex
        s
        " \t"
    )

isWs :: Word8 -> Bool
isWs s = isJust (B.elemIndex
        s
        " \t"
    )

http1Token :: Ap.Parser B.ByteString
http1Token = Ap.string "HTTP/1.1" <|> Ap.string "HTTP/1.0"

http1Method :: Ap.Parser B.ByteString
http1Method = 
    Ap.string "GET"
    <|> Ap.string "POST"
    <|> Ap.string "HEAD"
    <|> Ap.string "PUT"
    <|> Ap.string "OPTIONS"
    <|> Ap.string "TRACE"
    <|> Ap.string "CONNECT"

unspacedUri :: Ap.Parser B.ByteString
unspacedUri = Ap.takeWhile (not . isWs)


space :: Ap.Parser Word8
space = Ap.word8 32

requestLine :: Ap.Parser RequestOrResponseLine
requestLine = 
    flip Request_RoRL
    <$> 
    http1Method 
    <* space
    <*>
    unspacedUri 
    <* space
    <* http1Token

digit :: Ap.Parser Word8
digit = Ap.satisfy (Ap.inClass "0-9")

responseLine :: Ap.Parser RequestOrResponseLine
responseLine = 
    (pure Response_RoRL)
    <*
    http1Token
    <*
    space
    <*>
    ( read . map (toEnum . fromIntegral )  <$> Ap.count 3 digit )
    <*
    space 
    <*
    Ap.takeByteString

httpFirstLine :: Ap.Parser RequestOrResponseLine
httpFirstLine = requestLine <|> responseLine


headerListToHTTP11Text :: Headers -> Bu.Builder
headerListToHTTP11Text headers = 
    case headers of 
        -- According to the specs, :status can be only 
        -- the first header
        (hn,hv): rest | hn == ":status" ->
            (
                (first_line . read . unpack $ hv)
                `mappend`
                (go rest)
            )

        rest -> 
            (
                (first_line 200)
                `mappend`
                (go rest)
            )
  where 
    go [] = mempty
    go ((hn,hv):rest) = 
        (Bu.byteString hn) `mappend` ":" `mappend` " " `mappend` (Bu.byteString hv) 
                           `mappend` "\r\n" `mappend` (go rest)

    first_line :: Int -> Bu.Builder
    first_line code = mconcat [
        (Bu.byteString "HTTP/1.1"), " ",
        (Bu.string7 . show $ code), " ",
        (M.findWithDefault "OK" code httpStatusTable),
        "\r\n"
        ]



serializeHTTPResponse :: Headers -> [B.ByteString] -> Lb.ByteString
serializeHTTPResponse response_headers fragments = 
  let
    -- So got some data in an answer. Now there are three ways to go about 
    -- the returned data: to force a chunked transfer-encoding, to read all
    -- the data and add/set the Content-Length header, or to let the user 
    -- decide which one she prefers.
    --
    -- Right now I'm going for the second one, until somebody complains
    -- This is equivalent to a lazy byte-string...but I just need the 
    -- length 
    -- I promised to minimize the number of interventions of the library,
    -- so it could be a good idea to remove this one further down the 
    -- road. 
    h2 = E.lowercaseHeaders response_headers
    data_size = foldl' (\ n bs -> n + B.length bs) 0 fragments
    headers_editor = E.fromList h2
    content_length_header_lens = E.headerLens "content-length"
    he2 = L.set 
        content_length_header_lens 
        (Just (pack . show $ data_size)) 
        headers_editor 
    h3 = E.toList he2
    -- Next, I must serialize the headers....
    headers_text_as_builder = headerListToHTTP11Text h3

    -- We dump the headers first... unfortunately when talking 
    -- HTTP/1.1 the most efficient way to write those bytes is 
    -- to create a big buffer and pass it on to OpenSSL.
    -- However the Builder generating the headers above says 
    -- it generates fragments between 4k and 32 kb, I checked it
    -- and it is true, so we can use it

    -- Now we need to insert an extra \r\n, even it the response is 
    -- empty

    -- And then we use the builder to re-format the fragments returned
    -- by the coherent worker 
    -- TODO: This could be a good place to introduce chunked responses.
    body_builder = mconcat $ map Bu.byteString fragments



  in Bu.toLazyByteString $ headers_text_as_builder `mappend` "\r\n" `mappend`
                    body_builder


httpStatusTable :: M.Map Int Bu.Builder
httpStatusTable = M.fromList
    [
        (100, "Continue"),
        (101, "Switching Protocols"),
        (200, "OK"),
        (201, "Created"),
        (202, "Accepted"),
        (203, "Non-Authoritative Information"),
        (204, "No Content"),
        (205, "Reset Content"),
        (206, "Partial Content"),
        (300, "Multiple Choices"),
        (301, "Moved Permanently"),
        (302, "Found"),
        (303, "See Other"),
        (304, "Not Modified"),
        (305, "Use Proxy"),
        (307, "Temporary Redirect"),
        (400, "Bad Request"),
        (401, "Unauthorized"),
        (402, "Payment Required"),
        (403, "Forbidden"),
        (404, "Not Found"),
        (405, "Method Not Allowed"),
        (406, "Not Acceptable"),
        (407, "Proxy Authentication Required"),
        (408, "Request Timeout"),
        (409, "Conflict"),
        (410, "Gone"),
        (411, "Length Required"),
        (412, "Precondition Failed"),
        (413, "Request Entity Too Large"),
        (414, "Request-URI Too Long"),
        (415, "Unsupported Media Type"),
        (416, "Requested Range Not Satisfiable"),
        (417, "Expectation Failed"),
        (500, "Internal Server Error"),
        (501, "Not Implemented"),
        (502, "Bad Gateway"),
        (503, "Service Unavailable"),
        (504, "Gateway Timeout"),
        (505, "HTTP Version Not Supported")
    ]

-- For testing purposes... --------------------------------------------------------------
-----------------------------------------------------------------------------------------

-- assertEqual :: Eq a => String -> a -> a -> IO ()
-- assertEqual label v1 v2 = do 
--     putStrLn label
--     if v1 == v2 
--       then 
--         putStrLn "Ok"
--       else 
--         putStrLn "NoOk"
