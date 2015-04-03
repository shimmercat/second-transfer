{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, DeriveDataTypeable  #-} 
-- | A CoherentWorker is one that doesn't need to compute everything at once...
--   This one is simpler than the SPDY one, because it enforces certain order....



module SecondTransfer.MainLoop.CoherentWorker(
    getHeaderFromFlatList

    , Headers
    , FinalizationHeaders
    , Request
    , Footers
    , CoherentWorker
    , PrincipalStream
    , PushedStreams
    , PushedStream
    , DataAndConclusion
    , InputDataStream
    , StreamCancelledException (..)
    ) where 

import           Control.Exception
import qualified Data.ByteString   as B
import           Data.Conduit
import           Data.Foldable     (find)
import           Data.Typeable


-- |List of headers. The first part of each tuple is the header name 
-- (be sure to conform to the HTTP/2 convention of using lowercase)
-- and the second part is the headers contents. This list needs to include
-- the special :method, :scheme, :authority and :path pseudo-headers for 
-- requests; and :status (with a plain numeric value represented in ascii digits)
-- for responses.
type Headers = [(B.ByteString, B.ByteString)]

-- |This is a Source conduit (see Haskell Data.Conduit library from Michael Snoyman)
-- that you can use to retrieve the data sent by the client piece-wise.  
type InputDataStream = Source IO B.ByteString

-- | A request is a set of headers and a request body....
-- which will normally be empty, except for POST and PUT requests. But 
-- this library enforces none of that. 
type Request = (Headers, Maybe InputDataStream)

-- | Finalization headers. If you don't know what they are, chances are 
--   that you don't need to worry about them for now. The support in this 
--   library for those are at best sketchy. 
type FinalizationHeaders = Headers

-- | Finalization headers 
type Footers = FinalizationHeaders

-- | You use this type to answer a request. The `Headers` are thus response 
--   headers and they should contain the :status pseudo-header. The `PushedStreams`
--   is a list of pushed streams...(I don't thaink that I'm handling those yet)
type PrincipalStream = (Headers, PushedStreams, DataAndConclusion)


-- | A source-like conduit with the data returned in the response. The 
--   return value of the conduit is a list of footers. For now that list can 
--   be anything (even bottom), I'm not handling it just yet. 
type DataAndConclusion = ConduitM () B.ByteString IO Footers

-- | Main type of this library. You implement one of these for your server.
--   Basically this is a callback that the library calls as soon as it has
--   all the headers of a request. For GET requests that's the entire request
--   basically, but for POST and PUT requests this is just before the data 
--   starts arriving to the server. 
type CoherentWorker = Request -> IO PrincipalStream

-- | This exception will be raised inside a `CoherentWorker` when the underlying 
-- stream is cancelled (STREAM_RESET in HTTP\/2). Do any necessary cleanup
-- in a handler, or simply use the fact that the exception is asynchronously
-- delivered 
-- to your CoherentWorker Haskell thread, giving you an opportunity to 
-- interrupt any blocked operations.
data StreamCancelledException = StreamCancelledException
    deriving (Show, Typeable)

instance Exception StreamCancelledException

-- | A list of pushed streams 
type PushedStreams = [ IO PushedStream ]

-- | A pushed stream, represented by a list of request headers, 
--   a list of response headers, and the usual response body  (which 
--   may include final footers (not implemented yet)).
type PushedStream = (Headers, Headers, DataAndConclusion)

-- | Gets a single header from the list
getHeaderFromFlatList :: Headers -> B.ByteString -> Maybe B.ByteString
getHeaderFromFlatList unvl bs = 
    case find (\ (x,_) -> x==bs ) unvl of
        Just (_, found_value)  -> Just found_value 

        Nothing                -> Nothing  

