
module SecondTransfer.SpdyProtocol.Framing.WindowUpdate(
	WindowUpdateFrame(..)
	,WindowSizeValidFlags(..)
	) where 


import           Data.Binary            (Binary, get, put)
import           Data.Binary.Get        (getWord32be)
import           Data.Binary.Put        (putWord32be)
import           SecondTransfer.SpdyProtocol.Framing.Frame


data WindowSizeValidFlags = None_WSVF
	deriving (Show, Enum)


data WindowUpdateFrame = 
	WindowUpdateFrame {
		prologue:: ControlFrame WindowSizeValidFlags 
		, streamId:: Int 
		, deltaWindowSize:: Int
	}
	deriving Show



instance HasStreamId WindowUpdateFrame where 
    streamIdFromFrame = streamId 


instance Binary WindowUpdateFrame where 
	put wsf= do 
		let 
			new_prologue = resetControlFrameSize (prologue wsf) 16
		put $ new_prologue
		putWord32be $ fromIntegral $ streamId wsf 
		putWord32be $ fromIntegral $ deltaWindowSize wsf

	get = do 
		pr <- get
		stream_id <- getWord32be
		delta_window_size <- getWord32be
		return WindowUpdateFrame {
			prologue = pr
			, streamId = fromIntegral stream_id
			, deltaWindowSize = fromIntegral delta_window_size
		}
