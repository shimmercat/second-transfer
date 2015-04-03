{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module SecondTransfer.SpdyProtocol.Framing.KeyValueBlock(
    UnpackedNameValueList(..)
    ,CompressedKeyValueBlock(..)
    ,CompressedHeadersOnFrame(..)
    ) where 


import qualified Data.ByteString                as BS

import SecondTransfer.MainLoop.Tokens                     (
                                                UnpackedNameValueList(..) )
                                                -- ,packHeaderTuples
                                                -- ,unpackHeaderTuples )


data CompressedKeyValueBlock =
	-- To use raw
	 CompressedKeyValueBlock BS.ByteString
	-- Compromise: sometimes the headers can't be compressed until the very last 
	-- moment. 
	|UncompressedKeyValueBlock UnpackedNameValueList
    deriving Show


class CompressedHeadersOnFrame a where 
    getCompressedHeaders :: a ->  CompressedKeyValueBlock
    setCompressedHeaders :: a -> CompressedKeyValueBlock -> a
