{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs #-}


module SecondTransfer.SpdyProtocol.Framing.DataFrame(
	DataFrame(..)
	,DataFrameValidFlags(..)
	) where 


import           Data.Bits
-- import Data.Enum
import           Data.Binary                     (Binary, get, getWord8, put,
                                                  putWord8)
import           Data.Binary.Get                 (getByteString, getWord32be)
import           Data.Binary.Put                 (putByteString, putWord32be)
import           Data.BitSet.Generic             (delete, insert, member)
import qualified Data.ByteString                 as B
import           SecondTransfer.SpdyProtocol.Framing.Frame (FlagsBitSet,
                                                  HasFrameFlags (..),
                                                  HasStreamId   (..),
                                                  bitsetToWord8, word8ToBitset)
import           SecondTransfer.Utils                      (getWord24be, putWord24be)



data DataFrameValidFlags = Fin_F   -- Signals stream termination
	deriving (Show, Enum)


data DataFrame = 
	DataFrame {
		streamId:: Int
		, dataFrameFlags:: FlagsBitSet DataFrameValidFlags
		, payload:: B.ByteString
	}
	deriving Show


instance HasFrameFlags DataFrame DataFrameValidFlags where 
    -- applyFrameFlag :: frame -> flag -> Bool -> frame
    applyFrameFlag dataframe flag setornot = dataframe {
    		dataFrameFlags = newflags
		}
	  where 
	  	flags = dataFrameFlags dataframe
	  	newflags = if setornot then insert flag flags else delete flag flags

    -- getFrameFlag :: frame -> flag -> Bool
    getFrameFlag dataframe flag = 
    	member flag flags 
      where 
      	flags = dataFrameFlags dataframe


instance HasStreamId DataFrame where 
    -- streamIdFromFrame :: a -> Int
    streamIdFromFrame = streamId


instance Binary DataFrame where 
	put (DataFrame stream_id flags bs_payload) = 
	  do 
		putWord32be $ fromIntegral $ (stream_id .&. 2147483647)
		putWord8 $ bitsetToWord8 flags
		putWord24be $ B.length bs_payload 
		putByteString bs_payload

	get = 
	  do
	  	stream_id <- getWord32be
	  	flags_word <- getWord8
	  	data_length <- getWord24be
	  	bs_payload <- getByteString data_length
	  	return $ DataFrame
	  		(fromIntegral stream_id) 
	  		(word8ToBitset flags_word)
	  		bs_payload
