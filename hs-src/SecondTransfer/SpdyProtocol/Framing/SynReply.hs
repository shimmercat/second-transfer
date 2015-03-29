{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs #-}

module Rede.SpdyProtocol.Framing.SynReply(
	SynReplyValidFlags(..)
	,SynReplyFrame(..)
	) where 



import           Data.Binary                             (Binary, Get, get, put)
import           Data.Binary.Get                         (getByteString,
                                                          getWord32be)
import           Data.Binary.Put                         (putByteString,
                                                          putWord32be)
import           Data.BitSet.Generic                     (delete, insert,
                                                          member)
import qualified Data.ByteString                         as BS
import           Data.Default
import           Data.Word
import           Rede.SpdyProtocol.Framing.Frame
import           Rede.SpdyProtocol.Framing.KeyValueBlock (CompressedKeyValueBlock (..)
                                                          ,CompressedHeadersOnFrame (..) )



data SynReplyValidFlags = Fin_SRVF
    deriving (Show, Enum)



data SynReplyFrame = 
  SynReplyFrame {
    prologue:: ControlFrame SynReplyValidFlags
    , streamId:: Int
    -- To make sense of this, we need the state of the stream...
    , compressedKeyValueBlock:: CompressedKeyValueBlock
  } deriving Show


instance Default (ControlFrame SynReplyValidFlags) where 
    def = ControlFrame SynReply_CFT fbs0 0


instance HasFrameFlags SynReplyFrame SynReplyValidFlags where 

    applyFrameFlag frame flag set_value = frame {
        prologue = newpr }
      where 
        (ControlFrame cft flags len) = prologue frame 
        newpr = if set_value 
          then ControlFrame cft (insert flag flags) len
          else ControlFrame cft (delete flag flags) len 

    getFrameFlag frame flag = let 
        (ControlFrame _ flags _) = prologue frame 
      in member flag flags


instance HasStreamId SynReplyFrame where 
    streamIdFromFrame = streamId 

instance CompressedHeadersOnFrame SynReplyFrame where 
    getCompressedHeaders = compressedKeyValueBlock
    setCompressedHeaders synstreamframe compressedkvb = synstreamframe {
        compressedKeyValueBlock = compressedkvb
        }



instance Binary SynReplyFrame where

    put (SynReplyFrame pr strid (CompressedKeyValueBlock cmpkvb)) = do
        put newprologue 
        putWord32be (fromIntegral strid::Word32 )
        putByteString cmpkvb
      where 
        data_length = BS.length cmpkvb 
        total_data_length = 4 + data_length
        newprologue = resetControlFrameSize pr total_data_length

    get = do 
        pr <- get :: Get (ControlFrame SynReplyValidFlags)
        w32strid <- getWord32be 
        data_length <- return $ (cfLength pr) - 4 
        cmkvb <- getByteString data_length 
        return $ SynReplyFrame {
            prologue = pr
            ,streamId = fromIntegral w32strid
            ,compressedKeyValueBlock = CompressedKeyValueBlock cmkvb
        }

