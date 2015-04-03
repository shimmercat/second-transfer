{-# LANGUAGE OverloadedStrings #-}

module SecondTransfer.Constants(
	whichSPDY,
	whichHTTP,
	spdyDataFrameMaxLength
)  where

import           Data.ByteString(ByteString)

whichSPDY :: ByteString
whichSPDY = "spdy/3.1"

whichHTTP :: ByteString 
whichHTTP = "http/1.1"


spdyDataFrameMaxLength :: Int
spdyDataFrameMaxLength = 16384