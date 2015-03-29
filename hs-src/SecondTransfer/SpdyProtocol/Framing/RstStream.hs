{-# LANGUAGE FlexibleInstances #-}
module Rede.SpdyProtocol.Framing.RstStream (
	getRstStreamFrame
	,rstStreamFrame
	,getFrameResetReason

	,RstStreamFrame(..)
	,RstStreamValidFlags(..)
	,FrameResetReason(..) 
	) where 


import Rede.SpdyProtocol.Framing.Frame
import Data.BitSet.Generic(empty)
import           Data.Binary         (Binary,  get, put, Get)
-- import           Data.Binary.Builder (Builder)
import           Data.Binary.Put     (putWord32be)
import           Data.Binary.Get     (getWord32be)
import           Data.Default


data RstStreamValidFlags = None_F
	deriving (Show, Enum)

data RstStreamFrame = 
	RstStreamFrame {
		prologue:: ControlFrame RstStreamValidFlags 
		, streamId:: Int 
		, statusCode:: FrameResetReason
	}
	deriving Show


instance HasStreamId RstStreamFrame where
  streamIdFromFrame = streamId


instance Default (ControlFrame RstStreamValidFlags) where 
	def = ControlFrame RstStream_CFT (fbs1 None_F) 4


instance Binary RstStreamFrame where 
	put RstStreamFrame{prologue=pr, streamId=fi, statusCode=s} = 
	  do 
		put pr
		putWord32be $ fromIntegral fi
		putWord32be $ fromIntegral $ fromEnum s

	get = 
	  do
	  	pr  <- get 
	  	w32 <- getWord32be
	  	s   <- getWord32be
	  	return $ RstStreamFrame pr (fromIntegral w32) (toEnum $ fromIntegral s)


getFrameResetReason :: RstStreamFrame -> FrameResetReason
getFrameResetReason = statusCode


data FrameResetReason = Null_FRR 
	| ProtocolError_FRR
	| InvalidStream_FRR
	| RefusedStream_FRR
	| UnsuportedVersion_FRR
	| Cancel_FRR
	| InternalError_FRR
	| FlowControlError_FRR
	| StreamInUse_FRR
	| StreamAlreadyClosed_FRR
	| Deprecated1_FRR
	| FrameTooLarge_FRR

	deriving (Show, Enum, Eq)


getRstStreamFrame :: Get RstStreamFrame
getRstStreamFrame = get


rstStreamFrame :: Int -> FrameResetReason -> RstStreamFrame
rstStreamFrame  stream_id status_code = 
  RstStreamFrame 
    (ControlFrame RstStream_CFT empty 8)
    stream_id status_code