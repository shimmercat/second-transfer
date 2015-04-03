{-# LANGUAGE OverloadedStrings #-}

module SecondTransfer.Subprograms.BasicPing(
    basicPingProgram
    ) where


import qualified Data.ByteString           as B
import           Pipes
import qualified Pipes.Core                as PC
import           SecondTransfer.SpdyProtocol.Framing.AnyFrame
import           SecondTransfer.SpdyProtocol.Framing.Ping
import           SecondTransfer.MainLoop.Pipes
import           SecondTransfer.Utils
import qualified System.Clock              as SC
import           Control.Concurrent(threadDelay, killThread)
import qualified Pipes.Concurrent          as PC 
import           Data.IORef
import           GHC.Conc(STM)


data Event = 
      TimeGone_Ev 
    | FrameReceived_Ev AnyFrame
    deriving Show


enoughWaited :: Producer Event IO ()
enoughWaited = 
  do
    lift $ threadDelay 1000000  -- Wait 1 second
    yield (TimeGone_Ev)


framesProducer:: (IO B.ByteString ) -> PC.Producer AnyFrame IO ()
framesProducer gen = ( (chunkProducer gen "") >-> inputToFrames )


packetsEventProducer :: (IO B.ByteString ) -> PC.Producer Event IO ()
packetsEventProducer gen = 
    for (framesProducer gen) $ \x -> 
      do
        yield (FrameReceived_Ev x)


basicPingProgram :: IO B.ByteString  -> (B.ByteString -> IO () ) -> IO ()
basicPingProgram pullPacket pushPacket = 
  do
    (output, input, seal) <- PC.spawn' PC.unbounded 
    th1 <- PC.forkIO $ 
      do 
        runEffect $ (packetsEventProducer pullPacket) >-> PC.toOutput output
    th2 <- PC.forkIO $
      do 
        runEffect $ enoughWaited >-> PC.toOutput output 
    base_time     <- SC.getTime SC.Monotonic
    base_time_Ref <- newIORef base_time
    runEffect $ PC.fromInput input >-> (onEvent seal base_time_Ref) >-> framesToOutput >-> (outputConsumer pushPacket)
    killThread th1
    killThread th2


onEvent :: STM () ->IORef SC.TimeSpec ->  PC.Pipe Event AnyFrame IO ()
onEvent seal base_time  = 
  do
    ev <- await 
    case ev of
        TimeGone_Ev -> do
            t <- lift $ SC.getTime SC.Monotonic
            lift $ writeIORef base_time t
            yield $ AnyControl_AF $ PingFrame_ACF $ pingFrame 1
            onEvent seal base_time
        (FrameReceived_Ev (AnyControl_AF (PingFrame_ACF _))) -> do
            t <- lift $ readIORef base_time
            lift $ reportTimedEvent t "Got ping back"
            -- Don't recurse
            lift $ PC.atomically seal 
        (FrameReceived_Ev frame) -> do 
            -- Just print and discard the frame...
            lift $ putStrLn $ show frame 
            onEvent seal base_time 
