{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.MainLoop.Internal(
    readNextChunkAndContinue
    ,http2FrameLength
    ,InputFrame
    ) where


import SecondTransfer.MainLoop.Framer(readNextChunkAndContinue)
import SecondTransfer.Http2.Framer(http2FrameLength)
import SecondTransfer.Http2.Session(InputFrame)
