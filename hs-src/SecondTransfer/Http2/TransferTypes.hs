{-# LANGUAGE ExistentialQuantification, TemplateHaskell, DeriveDataTypeable, Rank2Types, OverloadedStrings #-}
module SecondTransfer.Http2.TransferTypes (
       SessionOutputPacket
     , SessionOutputCommand                             (..)
     , GlobalStreamId
     , SessionToFramerBlock                             (..)
     , OutputFrame
     , DataAndEffect
     ) where

--import Control.Lens

--import qualified Network.HPACK                          as HP
import qualified Data.ByteString                        as B
import qualified Network.HTTP2                          as NH2
import           Control.Concurrent                     (MVar)

import SecondTransfer.MainLoop.CoherentWorker           (
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


type DataAndEffect = (B.ByteString, Effect)


-- | Signals that the Session sends to the Framer concerning normal operation, including
--   high-priority data to send (in priority trains, already formatted), and a command to
--   start the output machinery for a new stream
data SessionToFramerBlock =
    Command_StFB         !SessionOutputCommand
  | PriorityTrain_StFB   [OutputFrame]
  | HeadersTrain_StFB (GlobalStreamId, [OutputFrame], MVar DataAndEffect) -- stream id, the headers of the response, the mvar where data is put.
--  | StartDataOutput_StFB (GlobalStreamId, MVar DataAndEffect )  -- ^ An empty string shall signal end of data


type SessionOutputPacket = SessionToFramerBlock
