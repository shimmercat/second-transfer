{-# LANGUAGE FunctionalDependencies, FlexibleInstances  #-} 
-- Contains adapter helpers to stream workers
module SecondTransfer.MainLoop.StreamPlug(
    UnpackedNameValueList
    ,StreamInputToken
    ,StreamOutputAction
    ,StreamId
    -- ,StreamPlug (..)
    ,IdStream(..)
    ) where 

-- import qualified Data.ByteString                as B



import SecondTransfer.MainLoop.Tokens

type StreamId = Int


class IdStream frame where 
    idStream :: frame -> Maybe StreamId 




