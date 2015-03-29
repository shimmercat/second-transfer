{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances  #-} 
-- | A CoherentWorker is one that doesn't need to compute everything at once...
--   This one is simpler than the SPDY one, because it enforces certain order....



module Rede.MainLoop.CoherentWorker(
    getHeaderFromFlatList

    , Headers
    , Request
    , Footers
    , CoherentWorker
    , PrincipalStream
    , PushedStreams
    , PushedStream
    , DataAndConclussion
    , InputDataStream
    ) where 


import Data.Conduit
import qualified Data.ByteString as B
import Data.Foldable(find)


type Headers = [(B.ByteString, B.ByteString)]

type InputDataStream = Source IO B.ByteString

-- A request is a set of headers and a request body....
-- which will normally be empty
type Request = (Headers, Maybe InputDataStream)

type FinalizationHeaders = Headers

-- Thanks to Luis Cobian for the name... 
type Footers = FinalizationHeaders

type CoherentWorker = Request -> IO PrincipalStream

type PrincipalStream = (Headers, PushedStreams, DataAndConclussion)

-- We can use this type in the future...
-- type DataAndConclussion = ConduitM () B.ByteString IO Footers

-- This type could possibly return footers but ... 
-- Update: there is functionality now to support this....
type DataAndConclussion = Source IO B.ByteString

type PushedStreams = [ IO PushedStream ]

type PushedStream = (Headers, DataAndConclussion)


getHeaderFromFlatList :: Headers -> B.ByteString -> Maybe B.ByteString
getHeaderFromFlatList unvl bs = 
    case find (\ (x,_) -> x==bs ) unvl of
        Just (_, found_value)  -> Just found_value 

        Nothing                -> Nothing  

