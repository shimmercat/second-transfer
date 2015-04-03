{-# LANGUAGE FlexibleInstances #-}

module SecondTransfer.SpdyProtocol.Framing.Headers(
	HeadersValidFlags(..)
	,HeadersFrame(..)
	) where 



import           Data.Word
import           Data.Binary                    (Binary, Get, get, put)
import           SecondTransfer.SpdyProtocol.Framing.Frame
import           Data.Binary.Get                (getWord32be, getByteString)
import           Data.Binary.Put                (putWord32be, putByteString)
import           SecondTransfer.SpdyProtocol.Framing.KeyValueBlock (CompressedKeyValueBlock(..), CompressedHeadersOnFrame(..))
import qualified Data.ByteString as BS
import           Data.Default


data HeadersValidFlags = None_HVF 
                         |Fin_HVF
    deriving (Show, Enum)



data HeadersFrame = 
  HeadersFrame {
    prologue:: ControlFrame HeadersValidFlags
    , streamId:: Int
    -- To make sense of this, we need the state of the stream...
    , compressedKeyValueBlock:: CompressedKeyValueBlock
  } deriving Show


instance Default (ControlFrame HeadersValidFlags) where 
    def = ControlFrame Headers_CFT (fbs1 None_HVF) 0


instance HasStreamId HeadersFrame where 
    streamIdFromFrame = streamId 


instance Binary HeadersFrame where

    put (HeadersFrame pr strid (CompressedKeyValueBlock cmpkvb)) = do
        put newprologue 
        putWord32be (fromIntegral strid::Word32 )
        putByteString cmpkvb
      where 
        data_length = BS.length cmpkvb 
        total_data_length = 4 + data_length
        newprologue = resetControlFrameSize pr total_data_length

    get = do 
        pr <- get :: Get (ControlFrame HeadersValidFlags)
        w32strid <- getWord32be 
        data_length <- return $ (cfLength pr) - 4 
        cmkvb <- getByteString data_length 
        return $ HeadersFrame {
            prologue = pr
            ,streamId = fromIntegral w32strid
            ,compressedKeyValueBlock = CompressedKeyValueBlock cmkvb
        }


instance CompressedHeadersOnFrame HeadersFrame where 
    getCompressedHeaders = compressedKeyValueBlock
    setCompressedHeaders synstreamframe compressedkvb = synstreamframe {
        compressedKeyValueBlock = compressedkvb
        }