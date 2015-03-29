{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs #-}


module Rede.SpdyProtocol.Framing.SynStream(
    SynStreamValidFlags(..)
    ,SynStreamFrame(..)
    ) where 


import           Data.Binary                             (Binary, Get, get, put)
import           Data.Binary.Get                         (getByteString,
                                                          getWord16be,
                                                          getWord32be)
import           Data.Binary.Put                         (putByteString,
                                                          putWord16be,
                                                          putWord32be)
import qualified Data.ByteString                         as BS
import           Data.BitSet.Generic                     (delete, insert,
                                                          member)
import           Data.Word
import           Data.Default


import           Rede.SpdyProtocol.Framing.Frame
import           Rede.SpdyProtocol.Framing.KeyValueBlock (

              CompressedHeadersOnFrame (..) 
              ,CompressedKeyValueBlock (..)
              )



data SynStreamValidFlags =  Fin_SSVF
                           |Unidirectional_SSVF
    deriving (Show, Enum)



data SynStreamFrame = 
  SynStreamFrame {
    prologue:: ControlFrame SynStreamValidFlags
    , streamId:: Int
    , associatedToStream:: Int 
    -- To make sense of this, we need the state of the stream...
    , compressedKeyValueBlock:: CompressedKeyValueBlock
  } deriving Show


instance HasStreamId SynStreamFrame where
  streamIdFromFrame = streamId

instance Default (ControlFrame SynStreamValidFlags) where 
    def = ControlFrame SynStream_CFT fbs0 0


instance HasFrameFlags SynStreamFrame SynStreamValidFlags where 
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


instance CompressedHeadersOnFrame SynStreamFrame where 
    getCompressedHeaders = compressedKeyValueBlock
    setCompressedHeaders synstreamframe compressedkvb = synstreamframe {
        compressedKeyValueBlock = compressedkvb
        }


instance Binary SynStreamFrame where

    put (SynStreamFrame pr strid assocstrid (CompressedKeyValueBlock cmpkvb)) = do
        put newprologue 
        putWord32be (fromIntegral strid::Word32 )
        putWord32be (fromIntegral assocstrid::Word32)
        putWord16be 0
        putByteString cmpkvb
      where 
        data_length = BS.length cmpkvb 
        total_data_length = 10 + data_length
        newprologue = resetControlFrameSize pr total_data_length

    get = do 
        pr <- get :: Get (ControlFrame SynStreamValidFlags)
        w32strid <- getWord32be 
        w32assocstrid <- getWord32be 
        getWord16be
        data_length <- return $ (cfLength pr) - 10  
        cmkvb <- getByteString data_length 
        return $ SynStreamFrame {
            prologue = pr
            ,streamId = fromIntegral w32strid
            ,associatedToStream = fromIntegral w32assocstrid
            ,compressedKeyValueBlock = CompressedKeyValueBlock cmkvb
        }

