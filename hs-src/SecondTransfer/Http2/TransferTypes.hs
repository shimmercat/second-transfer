{-# LANGUAGE ExistentialQuantification, TemplateHaskell, DeriveDataTypeable, Rank2Types, OverloadedStrings #-}
module SecondTransfer.Http2.TransferTypes (
       SessionOutputPacket
     , SessionOutputCommand                             (..)
     , GlobalStreamId
     , SessionToFramerBlock                             (..)
     , OutputFrame
     , OutputDataFeed
     , InputFrame
     , SessionInputCommand                              (..)
     ) where

--import Control.Lens

--import qualified Network.HPACK                          as HP
import qualified Data.ByteString                        as B
import qualified Network.HTTP2                          as NH2
import           Control.Concurrent                     (MVar)

import           System.Clock                            ( --getTime
                                                         --, Clock(..)
                                                         --, toNanoSecs
                                                         --, diffTimeSpec
                                                         TimeSpec
                                                         )


import           SecondTransfer.MainLoop.CoherentWorker  (
                                                        Effect(..)
                                                        )



type OutputFrame = (NH2.EncodeInfo, NH2.FramePayload, Effect)

type GlobalStreamId = Int

-- | Commands that the sessions sends to the framer which concern the whole
--   session
data  SessionOutputCommand =
    -- Command sent to the framer to close the session as harshly as possible. The framer
    -- will pick its own last valid stream.
    CancelSession_SOC !NH2.ErrorCodeId
    -- Command sent to the framer to close the session specifying a specific last-stream.
    | SpecificTerminate_SOC !GlobalStreamId !NH2.ErrorCodeId
  deriving Show


type OutputDataFeed = B.ByteString


-- | Signals that the Session sends to the Framer concerning normal operation, including
--   high-priority data to send (in priority trains, already formatted), and a command to
--   start the output machinery for a new stream
data SessionToFramerBlock =
    Command_StFB         !SessionOutputCommand
  | PriorityTrain_StFB   [OutputFrame]
  | HeadersTrain_StFB (GlobalStreamId, [OutputFrame], Effect, MVar OutputDataFeed)
                      -- stream id, the headers of the response,
                       -- the effect provided by the worker, the mvar where data is put.
--  | StartDataOutput_StFB (GlobalStreamId, MVar DataAndEffect )  -- ^ An empty string shall signal end of data


type SessionOutputPacket = SessionToFramerBlock




-- |Have to figure out which are these...but I would expect to have things
-- like unexpected aborts here in this type.
data SessionInputCommand =
    FirstFrame_SIC InputFrame               -- | This frame is special
    |MiddleFrame_SIC InputFrame             -- | Ordinary frame
    |InternalAbort_SIC                      -- | Internal abort from the session itself
    |InternalAbortStream_SIC GlobalStreamId  -- | Internal abort, but only for a frame
    |CancelSession_SIC                      -- |Cancel request from the framer
    |PingFrameEmitted_SIC (Int, TimeSpec)   -- |The Framer decided to emit a ping request, this is the sequence number (of the packet it was sent on) and the time
  deriving Show



type InputFrame  = NH2.Frame
