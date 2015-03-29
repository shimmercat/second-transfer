{-# LANGUAGE FunctionalDependencies, FlexibleInstances  #-} 
-- Contains adapter helpers to stream workers
module Rede.MainLoop.StreamPlug(
    UnpackedNameValueList
    ,StreamInputToken
    ,StreamOutputAction
    ,StreamId
    -- ,StreamPlug (..)
    ,IdStream(..)
    ) where 

-- import qualified Data.ByteString                as B



import Rede.MainLoop.Tokens

type StreamId = Int


class IdStream frame where 
    idStream :: frame -> Maybe StreamId 




