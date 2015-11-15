{-# LANGUAGE ExistentialQuantification, TemplateHaskell, DeriveDataTypeable, Rank2Types, OverloadedStrings #-}
module SecondTransfer.Http2.TransferTypes (
       SessionOutputPacket
     , SessionOutputCommand                             (..)
     , GlobalStreamId
     , SessionToFramerBlock                             (..)
     , OutputFrame
     ) where

--import Control.Lens

--import qualified Network.HPACK                          as HP
import qualified Network.HTTP2                          as NH2

import SecondTransfer.MainLoop.CoherentWorker           (
                                                        Effect(..)
                                                        )



type OutputFrame = (NH2.EncodeInfo, NH2.FramePayload, Effect)

type GlobalStreamId = Int

-- Session output commands indicating something to the Framer
data  SessionOutputCommand =
    -- Command sent to the framer to close the session as harshly as possible. The framer
    -- will pick its own last valid stream.
    CancelSession_SOC !NH2.ErrorCodeId
    -- Command sent to the framer to close the session specifying a specific last-stream.
    |SpecificTerminate_SOC !GlobalStreamId !NH2.ErrorCodeId
    -- Command sent to the framer to notify it of a normal end of stream
    -- condition, this is used to naturally close each stream.
    |FinishStream_SOC !GlobalStreamId
  deriving Show


data SessionToFramerBlock =
    Command_StFB        !SessionOutputCommand
  | PriorityTrain_StFB  [OutputFrame]
  | DataFrame_StFB      !OutputFrame
    deriving Show

type SessionOutputPacket = SessionToFramerBlock
