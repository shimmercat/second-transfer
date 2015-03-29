{-# LANGUAGE OverloadedStrings #-}
module Rede.MainLoop.Pipes (
    showFrames
    ,inputToFrames
    ,framesToOutput
    ,outputConsumer
    ,chunkProducer) where 


import           Data.Binary.Put                         (runPut)
import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                    as LB
import           Pipes
import qualified Pipes.Core                              as PC
import           Rede.SpdyProtocol.Framing.ChunkProducer (chunkProducerHelper)


import           Rede.SpdyProtocol.Framing.AnyFrame      (AnyFrame (..),
                                                          perfunctoryClassify,
                                                          readFrame, writeFrame)



chunkProducer :: IO B.ByteString    -- Generator
      -> LB.ByteString              -- Left-overs
      -> PC.Producer LB.ByteString IO ()
chunkProducer gen leftovers = do 
    (bytes_of_frame, new_leftovers) <- lift $ chunkProducerHelper leftovers gen Nothing
    yield bytes_of_frame
    chunkProducer gen new_leftovers




-- Now let's define a pipe that converts ByteString representations of frames 
-- to AnyFrame
inputToFrames :: PC.Pipe LB.ByteString AnyFrame IO ()
inputToFrames = 
  do
    the_bytes <- await
    perfunct_classif <- return $ perfunctoryClassify the_bytes
    the_frame  <- return $ readFrame the_bytes perfunct_classif 
    yield the_frame 
    inputToFrames


framesToOutput :: PC.Pipe AnyFrame LB.ByteString IO ()
framesToOutput =
  do 
    the_frame <- await
    the_bytes <- return $ runPut $ writeFrame the_frame
    yield the_bytes
    framesToOutput


outputConsumer :: (B.ByteString -> IO () ) -> PC.Consumer LB.ByteString IO ()
outputConsumer pushToWire = 
  do
    lazy_bytes <- await 
    lift $ pushToWire $ LB.toStrict lazy_bytes
    outputConsumer pushToWire


-- For debugging
showFrames :: IO B.ByteString -> IO ()
showFrames gen = runEffect $ framesPrint gen


framesPrint :: IO B.ByteString -> PC.Effect IO ()
framesPrint gen = 
    for framesProducer $ \ frame -> do 
        lift $ putStrLn $ show frame
  where 
    framesProducer = (chunkProducer gen "") >-> inputToFrames

