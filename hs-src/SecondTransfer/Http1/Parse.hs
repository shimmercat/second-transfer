{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module SecondTransfer.Http1.Parse(
    newIncrementalHttp1Parser
    ,addBytes

    -- Internal exports, used by the test suite
    ,locateCRLFs
    ,splitByColon
    ,stripBs
    ,headerListToHTTP1RequestText
    ,headerListToHTTP1ResponseText
    ,serializeHTTPResponse
    ,methodHasRequestBody
    ,methodHasRequestBody'
    ,methodHasResponseBody
    ,chunkParser
    ,transferEncodingIsChunked
    ,wrapChunk
    ,unwrapChunks
    ,leftoversFromParserCompletion
    ,responseStatusHasResponseBody

    ,IncrementalHttp1Parser
    ,Http1ParserCompletion(..)
    ,BodyStopCondition(..)
    ) where



import           Control.Exception                      (throw)
import           Control.Lens
import qualified Control.Lens                           as L
import           Control.Applicative
--import           Control.DeepSeq                        (deepseq)

import           GHC.Stack

import           Numeric                                as Nm

import qualified Data.ByteString                        as B
import           Data.List                              (foldl')
import qualified Data.ByteString.Builder                as Bu
import           Data.ByteString.Char8                  (pack, unpack)
import qualified Data.ByteString.Char8                  as Ch8
import qualified Data.ByteString.Lazy                   as Lb
import qualified Data.ByteString.Lazy                   as LB
import           Data.Char                              (toLower, isSpace)
import           Data.Maybe                             (isJust, fromMaybe)

import           GHC.Stack

import qualified Data.Attoparsec.ByteString             as Ap
import qualified Data.Attoparsec.ByteString.Char8       as Ap8

import           Data.Foldable                          (find)
import           Data.Word                              (Word8)
import qualified Data.Map                               as M
import           Data.Conduit

import           Text.Read                              (readEither)

import qualified Network.URI                            as U

import           SimpleHttpHeadersHq

import qualified SecondTransfer.Utils.HTTPHeaders       as He
import           SecondTransfer.Exception
import           SecondTransfer.Utils                   (subByteString)
import qualified SecondTransfer.ConstantsAndLimits      as Constant

-- import           Debug.Trace


data IncrementalHttp1Parser = IncrementalHttp1Parser {
    _fullText :: Bu.Builder
    ,_stateParser :: HeaderParseClosure
    }


type HeaderParseClosure = (LB.ByteString ->  ([Int], Int, Word8))

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
    MustContinue_H1PC !IncrementalHttp1Parser
    -- | Headers were completed. For some HTTP methods that's all
    --   there is, and that's what this case represents. The second
    --   argument is a left-overs string, that should be completed
    --   with any other data required
    |OnlyHeaders_H1PC  !HqHeaders !B.ByteString
    -- | For requests with a body. The second argument is a condition
    --   to stop receiving the body, the third is leftovers from
    --   parsing the headers.
    |HeadersAndBody_H1PC  !HqHeaders !BodyStopCondition !B.ByteString
    -- | Some requests are ill-formed. We can check those cases
    --   here.
    |RequestIsMalformed_H1PC String
    deriving Show


leftoversFromParserCompletion :: Http1ParserCompletion -> B.ByteString
leftoversFromParserCompletion (OnlyHeaders_H1PC _ l) = l
leftoversFromParserCompletion (HeadersAndBody_H1PC _ _ l) = l
leftoversFromParserCompletion _ = mempty

-- | Stop condition when parsing the body.
--  Tons and tons of messages in the internet go without a Content-Length
--  header, in those cases there is a long chain of conditions to determine the
--  message length, and at the end of those, there is CloseConnection
--
data BodyStopCondition =
     UseBodyLength_BSC Int          -- ^ There is a content-length header, and a length
   | ConnectionClosedByPeer_BSC     -- ^ We expect the connection to be closed by the peer when the stream finishes
   | Chunked_BSC                    -- ^ It's a chunked transfer, use the corresponding parser
   | SemanticAbort_BSC              -- ^ Terrible things have happened, close the connection
     deriving (Show, Eq)


-- | What can we parse from the first line?
data FirstLineDatum =
    -- | First argument is the URI, second the method
    Request_RoRL B.ByteString B.ByteString
    -- | First argument is the status code
    |Response_RoRL Int
    -- | First line is just part of the mime message , this is used
    --   by HTTP/1.1. First argument is the header "name", second
    --   is the header value.
    |NormalMime_RoRL B.ByteString B.ByteString
    deriving (Show, Eq)


addBytes :: IncrementalHttp1Parser -> LB.ByteString -> Http1ParserCompletion
addBytes (IncrementalHttp1Parser full_text header_parse_closure) new_bytes =
  let -- Just feed the bytes
    (positions, length_so_far, last_char ) = header_parse_closure new_bytes
    new_full_text = full_text `mappend` (Bu.lazyByteString new_bytes)
    could_finish = twoCRLFsAreConsecutive positions
    total_length_now = fromIntegral (LB.length new_bytes) + length_so_far
    full_text_lbs = (Bu.toLazyByteString new_full_text)
    -- This will only trigger for ill-formed heads, if the head is parsed successfully, this
    -- flag will be ignored.
    head_is_suspicious =
      if total_length_now > 399 then
         if total_length_now < Constant.maxUrlLength
            then looksSuspicious full_text_lbs
            else True
      else False
  in
    case (could_finish, head_is_suspicious) of
        (Just at_position, _) -> elaborateHeaders new_full_text positions at_position

        (Nothing,      True ) -> RequestIsMalformed_H1PC "Head is suspicious"

        (Nothing,      False) -> MustContinue_H1PC
                    $ IncrementalHttp1Parser
                        new_full_text
                        (locateCRLFs length_so_far positions last_char)


-- Look for suspicious patterns in the bs, like tab characters, or \r or \n
-- which are alone
looksSuspicious :: Lb.ByteString -> Bool
looksSuspicious bs  =
  let
    have_weird_characters = isJust $ Lb.find (\w8 -> w8 < 32 && w8 /= 10 && w8 /= 13 ) bs
    have_lone_n  = let
        ei = Lb.elemIndices 13 bs
        eii = Lb.elemIndices 10 bs
        zp = zip ei eii
        f ((i,j):rest)  | i+1 == j = f rest
                        | i == Lb.length bs - 1 = False
                        | otherwise            = True
        f []                                   = False
        in  f zp || abs ( length ei - length eii) > 1
    result = have_lone_n || have_weird_characters
  in  result


-- This function takes care of retrieving headers....
elaborateHeaders :: HasCallStack => Bu.Builder -> [Int] -> Int -> Http1ParserCompletion
elaborateHeaders full_text crlf_positions last_headers_position =
  let
    -- Start by getting a full byte-string representation of the headers,
    -- no need to be silly with chunks.
    full_headers_text = Lb.toStrict $ Bu.toLazyByteString full_text

    -- Filter out CRLF pairs corresponding to multiline headers.
    no_cont_positions_reverse =
        filter
            (\ pos -> if pos == last_headers_position then True else
                if pos > last_headers_position then False else
                    not . isWsCh8 $
                        (Ch8.index
                            full_headers_text
                            (pos + 2)
                        )
            )
            crlf_positions

    no_cont_positions = reverse no_cont_positions_reverse

    -- Now get the headers as slices from the original string.
    headers_pre :: [B.ByteString]
    headers_pre = map
        (\ (start, stop) ->
            subByteString start stop full_headers_text
        )
        (zip
            ((:)
                0
                (map
                    ( + 2 )
                    (init no_cont_positions)
                )
            )
            no_cont_positions
        )

    no_empty_headers ::[B.ByteString]
    no_empty_headers = filter (\x -> B.length x > 0)  headers_pre

    -- We remove the first "header" because it is actually the
    -- initial HTTP request/response line
    headers_0 =  map splitByColon $ tail  no_empty_headers

    -- The first line is not actually a header, but contains the method, the version
    -- and the URI
    maybe_request_or_response = parseFirstLine (head headers_pre)

    headers_1 =  [
        ( (stripBsHName . bsToLower $ hn), stripBs hv ) | (hn, hv) <- headers_0
        ]

    Just request_or_response = maybe_request_or_response

    (headers_2, has_body) = case request_or_response of

        Request_RoRL uri method ->
          let
            -- No lowercase, methods are case sensitive
            -- lc_method = bsToLower method
            --
            -- TODO: There is a bug here, according to Section 3.3 of RFC 7230
            has_body' = methodHasRequestBody method
          in
            -- TODO:  We should probably add the "scheme" pseudo header here
            ( (":path", uri):(":method",method):headers_1, has_body' )

        Response_RoRL status ->
          let
            status_str = pack . show $ status
            excludes_body =
                ( (Ch8.head status_str) == '1')
                ||
                ( status == 204 || status == 304)
          in
            ((":status", status_str): headers_1, not excludes_body)

        NormalMime_RoRL hn hv ->
          let
            headers_interin =
                (bsToLower hn, hv):headers_1
            (status_str, hh) = case lookup "status" headers_interin of
                Nothing -> ("200", headers_interin)
                Just x ->  (x,
                               filter
                                   (
                                   \(hhn, _hv) -> hhn /= "status")
                                   headers_interin
                            )
            excludes_body =
                ( (Ch8.head status_str) == '1')
                ||
                ( status_str == "204" || status_str == "304" )

          in ((":status", takeFirstPartOfStatus status_str): hh, not excludes_body)


    -- Still we need to lower-case header names, and trim them
    headers_3 = [
        ( (stripBsHName . bsToLower $ hn), stripBs hv ) | (hn, hv) <- headers_2
        ]

    -- TODO: Find out what to do with header parse errors
    (headers_hq, _parse_error_list) = parseFromTupleList headers_3

    content_stop :: BodyStopCondition
    content_stop =
      let
        cnt_length_header = find (\ x -> (fst x) == "content-length" )  headers_3
        transfer_encoding = find (\ x -> (fst x) == "transfer-encoding" ) headers_3
      in
        case transfer_encoding of
            Nothing ->
              case cnt_length_header of
                  Just (_, hv) -> case readEither . unpack $ hv of
                     Left _ -> SemanticAbort_BSC
                     Right n -> UseBodyLength_BSC n
                  Nothing -> ConnectionClosedByPeer_BSC

            Just (_, tre_value)
              | transferEncodingIsChunked tre_value ->
                  Chunked_BSC
              | otherwise ->
                  SemanticAbort_BSC


    leftovers = B.drop (last_headers_position + 4) full_headers_text
    all_headers_ok = all verifyHeaderSyntax headers_1
  in
    if isJust maybe_request_or_response then
        (if all_headers_ok then
            if has_body
              then
                HeadersAndBody_H1PC headers_hq content_stop leftovers
              else
                OnlyHeaders_H1PC headers_hq leftovers
        else
            RequestIsMalformed_H1PC "InvalidSyntaxOnHeaders")
    else
        RequestIsMalformed_H1PC "InvalidFirstLineOnRequest"


splitByColon :: HasCallStack => B.ByteString -> (B.ByteString, B.ByteString)
splitByColon  = L.over L._2 (B.tail) . Ch8.break (== ':')


transferEncodingIsChunked :: B.ByteString -> Bool
transferEncodingIsChunked x = x == "chunked"


verifyHeaderName :: B.ByteString -> Bool
verifyHeaderName =
  B.all ( \ w8 ->
     (    ( w8 >= 48 && w8 <=57 ) || ( w8 >= 65 && w8 <= 90)
       || ( w8 >= 97 && w8 <= 122) )
       || ( w8 == 43 ) || ( w8 == 95)   -- "extensions" for our local tooling
       || ( w8 == 45)  -- Standard dash
  )


verifyHeaderValue :: B.ByteString -> Bool
verifyHeaderValue =   B.all ( \ w8 ->
   w8 >= 32 && w8 < 127
  )


verifyHeaderSyntax :: (B.ByteString, B.ByteString) -> Bool
verifyHeaderSyntax (a,b) = verifyHeaderName a && verifyHeaderValue b


parseFirstLine :: B.ByteString -> Maybe FirstLineDatum
parseFirstLine s =
  let
    either_error_or_rrl = Ap.parseOnly (httpFirstLine <* Ap.endOfInput ) s
  in
    case either_error_or_rrl of
        Left _ -> Nothing
        Right rrl -> Just rrl


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


stripBsHName :: B.ByteString -> B.ByteString
stripBsHName s =
   Ch8.dropWhile isWsCh8 s


locateCRLFs :: Int -> [Int] -> Word8 ->  LB.ByteString ->  ([Int], Int, Word8)
locateCRLFs initial_offset other_positions prev_last_char next_chunk =
  let
    (last_char, positions_list, strlen) =
        LB.foldl
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


-- Parses the given list of positions, which is a reversed list. If
-- we find that the two latest positions of CRLF are consecutive,
-- then we are ok. and return it
twoCRLFsAreConsecutive :: [Int] -> Maybe Int
twoCRLFsAreConsecutive  positions =
  let
    -- This function is moving from tail to head
    go  seen (p2:p1:r) | p2 - p1 == 2         = go (Just p1)  (p1:r)
                       | otherwise            = go seen       (p1:r)
    go  seen _                                = seen
  in go Nothing positions


isWsCh8 :: Char -> Bool
isWsCh8 ch = isJust (Ch8.elemIndex
        ch
        " \t"
    )


isWs :: Word8 -> Bool
isWs ch =  (ch == 32) || (ch == 9)


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


requestLine :: Ap.Parser FirstLineDatum
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


safeStringToInt :: HasCallStack => String -> Int
safeStringToInt s =
    case readEither s of
        Left _ -> throw (HTTP11SyntaxException $ "BadDigits found via stack: " ++ prettyCallStack callStack)
        Right n -> n


responseLine :: HasCallStack => Ap.Parser FirstLineDatum
responseLine =
    (pure Response_RoRL)
    <*
    http1Token
    <*
    space
    <*>
    ( safeStringToInt . map (toEnum . fromIntegral )  <$> Ap.count 3 digit )
    <*
    space
    <*
    Ap.takeByteString

classStuff :: String
classStuff =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-"

-- Another type of first line
normalMimeLine :: Ap.Parser FirstLineDatum
normalMimeLine =
    (pure NormalMime_RoRL )
    <*
    (Ap.many' space)
    <*>
    Ap.takeWhile1 ( Ap.inClass classStuff )
    <*
    (Ap.many' space)
    <*
    Ap8.char ':'
    <*
    (Ap.many' space)
    <*>
    (fst . Ch8.spanEnd isSpace <$> Ap8.takeByteString )


takeFirstPartOfStatus :: B.ByteString -> B.ByteString
takeFirstPartOfStatus s = B.takeWhile
   (\ w -> w >= (fromIntegral $ fromEnum '0')
           &&
          w <=  (fromIntegral $ fromEnum '9')
   )
   s


httpFirstLine :: Ap.Parser FirstLineDatum
httpFirstLine = requestLine <|> responseLine <|> normalMimeLine

-- A parser for chunked  messages ....
chunkParser :: Ap.Parser B.ByteString
chunkParser = do
    lng_bs <- Ap8.hexadecimal :: Ap.Parser Int
    Ap.option () (
      do
        _ <- Ap.sepBy (Ap8.many1 $ Ap8.satisfy (Ap8.notInClass ";\r\n") ) (Ap8.char ';')
        return ()
        )
    _ <- Ap8.char '\r'
    _ <- Ap8.char '\n'
    cnt <- Ap.take lng_bs
    _ <- Ap8.char '\r'
    _ <- Ap8.char '\n'
    return cnt


wrapChunk :: B.ByteString -> Lb.ByteString
wrapChunk bs = let
    lng = B.length bs
    lng_str = showHex lng ""
    a0 = Bu.byteString . pack $ lng_str
    a1 = Bu.byteString bs
    in Bu.toLazyByteString $ a0 `mappend` "\r\n" `mappend` a1 `mappend` "\r\n"


unwrapChunks :: Monad m => Conduit B.ByteString m B.ByteString
unwrapChunks  =
    do
      -- Leftovers are fed and they will be read here.
      input <- await
      case {- trace ("CHNK iNPUT:" ++ show input)  -}  input  of
          Nothing -> return ()
          Just bs ->
              let
                  parse_result = Ap.parse chunkParser bs
              in onresult parse_result
  where
    onresult parse_result =
        case parse_result of
            Ap.Fail  _ _  _ -> throw $ HTTP11SyntaxException "ChunkedParsingFailed"
            Ap.Partial fn -> go fn
            Ap.Done leftovers payload -> do
                payload `seq` yield payload
                if (B.length payload > 0)
                  then
                    restart leftovers
                  else
                    leftover leftovers
    go fn = do
      input <- await
      case {- trace ( "CHNK Input: " ++ show input)  $  -} input of
          Nothing -> do
              -- Due to buggy pears, we have to be more accepting
              -- throw $ HTTP11SyntaxException "ChunkedParsingLeftUnfinished"
              yield ""
              return ()
          Just bs ->
              let
                  parse_result = fn bs
              in onresult parse_result
    restart leftovers =
        let
            parse_result = Ap.parse chunkParser leftovers
        in onresult parse_result


-- | This is a serialization function: it goes from content to string
-- It is not using during parse, but during the inverse process.
-- This function adds a single \r\n at the end of the output
headerListToHTTP1ResponseText :: HasCallStack => HqHeaders -> Bu.Builder
headerListToHTTP1ResponseText headers =
    case  headers ^. serialized_HqH  of
        -- According to the specs, :status can be only
        -- the first header
        (hn,hv): rest | hn == ":status" ->
            (
                (first_line . safeStringToInt . unpack $ hv)
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


-- | Converts a list of headers to a request head.
-- Invoke with the request data. Don't forget to clean the headers first.
-- NOTICE that this function doesn't add the \r\n extra-token for the empty
-- line at the end of headers.
headerListToHTTP1RequestText :: HqHeaders -> Bu.Builder
headerListToHTTP1RequestText  headers =
    expressAsHTTP1RequestHeaderBlock headers'
  where
    host_header = case headers ^. authority_Hi of
        Nothing ->  headers ^. host_Hi
        a@(Just some_auth) -> a
    headers' =
        (set host_Hi  host_header) .
        (set authority_Hi Nothing) $
        headers


-- -- | Converts a list of headers to a request head.
-- -- Invoke with the request data. Don't forget to clean the headers first.
-- -- NOTICE that this function doesn't add the \r\n extra-token for the empty
-- -- line at the end of headers.
-- headerListToHTTP1RequestText :: HqHeaders -> Bu.Builder
-- headerListToHTTP1RequestText headers =
--     go1 Nothing Nothing mempty (headers ^. serialized_HqH)
--   where
--     go1 mb_method mb_local_uri assembled_body [] =
--         (fromMaybe "GET" mb_method) `mappend` " " `mappend` (fromMaybe "*" mb_local_uri) `mappend` " " `mappend` "HTTP/1.1" `mappend` "\r\n"
--           `mappend` assembled_body

--     go1 _           mb_local_uri assembled_body ((hn,hv): rest)
--       | hn == ":method" =
--           go1 (Just . Bu.byteString . validMethod $ hv) mb_local_uri assembled_body rest

--     go1 mb_method   _mb_local_uri    assembled_body ((hn,hv): rest)
--       | hn == ":path"   =
--           go1 mb_method  (Just . Bu.byteString . cleanupAbsoluteUri $ hv) assembled_body rest

--     -- Authority pseudo-header becomes a host header.
--     go1 mb_method   _mb_local_uri    assembled_body ((hn,hv): rest)
--       | hn == ":authority"   =
--           go1 mb_method  (Just . Bu.byteString . cleanupAbsoluteUri $ hv) (assembled_body `mappend` "host" `mappend` ":" `mappend` (Bu.byteString hv) `mappend` "\r\n") rest

--     go1 mb_method mb_local_uri assembled_body ((hn,hv):rest)   -- Ignore any strange pseudo-headers
--       | He.headerIsPseudo hn = go1 mb_method mb_local_uri assembled_body rest
--       | otherwise = go1 mb_method mb_local_uri (assembled_body `mappend` (Bu.byteString hn) `mappend` ":" `mappend` (Bu.byteString hv) `mappend` "\r\n") rest


-- | Function used for testing....
serializeHTTPResponse :: HqHeaders -> [B.ByteString] -> Lb.ByteString
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
    data_size = foldl' (\ n bs -> n + B.length bs) 0 fragments
    h2 = L.set
        contentLength_Hi
        (Just . fromIntegral $ data_size )
        response_headers

    -- Next, I must serialize the headers....
    headers_text_as_builder = headerListToHTTP1ResponseText h2

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


validMethod :: B.ByteString -> B.ByteString
validMethod mth | mth == "GET"     =  mth
                | mth == "POST"    =  mth
                | mth == "HEAD"    =  mth
                | mth == "OPTIONS" =  mth
                | mth == "PUT"     =  mth
                | mth == "DELETE"  =  mth
                | mth == "TRACE"   =  mth
                | otherwise        = "GET"


methodHasRequestBody :: B.ByteString -> Bool
methodHasRequestBody mth | mth == "GET"     =  False
                         | mth == "POST"    =  True
                         | mth == "HEAD"    =  False
                         | mth == "OPTIONS" =  False
                         | mth == "PUT"     =  True
                         | mth == "DELETE"  =  False
                         | mth == "TRACE"   =  False
                         | otherwise        =  False


methodHasRequestBody' :: HttpMethod -> Bool
methodHasRequestBody' mth = case  mth of
    Get_HtM     ->  False
    Post_HtM    ->  True
    Head_HtM    ->  False
    Options_HtM ->  False
    Put_HtM     ->  True
    Delete_HtM  ->  False


-- These are most likely wrong TODO: fix
methodHasResponseBody :: B.ByteString -> Bool
methodHasResponseBody mth | mth == "GET"     = True
                         | mth == "POST"    =  True
                         | mth == "HEAD"    =  False
                         | mth == "OPTIONS" =  False
                         | mth == "PUT"     =  True
                         | mth == "DELETE"  =  True
                         | mth == "TRACE"   =  False
                         | otherwise        =  False


responseStatusHasResponseBody :: Int -> Bool
responseStatusHasResponseBody code
  | code == 204                     = False
  | code == 304                     = False
  | otherwise                       = True


cleanupAbsoluteUri :: HasCallStack => B.ByteString -> B.ByteString
-- Just trigger a 404 with an informative message (perhaps)
cleanupAbsoluteUri u
  | B.length u == 0
     = "/client-gives-invalid-uri/"
  | B.head u /= 47
     = "/client-gives-invalid-uri/"
  | otherwise
     =
       let
           str = unpack u
           ok = U.isRelativeReference str
       in if ok then u else "/client-gives-invalid-uri/"




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
