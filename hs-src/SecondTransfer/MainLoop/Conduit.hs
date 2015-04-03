{-# LANGUAGE OverloadedStrings, ExistentialQuantification, Rank2Types,
             MultiParamTypeClasses, FunctionalDependencies
 #-}


module SecondTransfer.MainLoop.Conduit ( 
  activateSessionManager
  ,FramesInterface(..)
  ) where 


import           Control.Monad.Trans.Class(lift)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Concurrent
import qualified Data.Conduit                 as C
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB

import           SecondTransfer.MainLoop.Framer(Framer)
import           SecondTransfer.MainLoop.PushPullType


--  Generalized Session type....
--  frames to engine      ---v  frames from engine---v
type SessionM m frame = m ( (C.Sink frame m () ), (C.Source m frame ) )


-- | This is a correct ByteString slicer: it takes ByteStrings from some 
--   generating action, and produces a source of ByteStrings... the ByteStrings
--   emerging from the source have each the correct size for a frame.
chunkProducer :: Monad m => m B.ByteString    -- Generator
      -> LB.ByteString                        -- Left-overs
      -> Framer m                             -- Frame-specific helpers
      -> C.Source m LB.ByteString             -- Source m LB.ByteString
chunkProducer gen leftovers chunk_producer_helper = do 
    (bytes_of_frame, new_leftovers) <- lift $ chunk_producer_helper leftovers gen Nothing
    C.yield bytes_of_frame
    chunkProducer gen new_leftovers chunk_producer_helper


class Monad m => FramesInterface m frame | frame -> m where
  inputToFrames  :: C.Conduit LB.ByteString m frame 
  framesToOutput :: C.Conduit frame m LB.ByteString


-- data Monad m => FramesInterface m frame = FramesInterface {
--    inputToFrames  :: C.Conduit LB.ByteString m frame 
--    ,framesToOutput :: C.Conduit frame m LB.ByteString
--   }
 

outputConsumer :: Monad m => (LB.ByteString -> m () ) -> C.Sink LB.ByteString m ()
outputConsumer pushToWire = CL.mapM_ pushToWire


-- | Creates the session source and the session sink. The session source is where 
--   output frames appear, and the session sink is where input frames dissapear (this, 
--   is is the entrance to the session manager). The session sink runs in its own 
--   thread, while the session source runs in this thread. This manager exits when 
--   the session source exits.
activateSessionManager :: 
    (MonadIO m, FramesInterface m frame) => 
    (forall a . m a -> IO a) ->          -- Session start: translates actions in the m monad to IO actions

    SessionM m frame->                   -- Session: a function that creates a source and a sink of frames. 
                                         --    the session is the "inner engine" that processes the frames, 
                                         --    do not confuse with the outer translation layer
    PushAction      ->                   --  Puts outputs in the wire
    PullAction      ->                   --  Takes inputs from the wire
    Framer m        ->                   
    IO () 
activateSessionManager session_start session push pull chunk_producer_helper = let

    input_to_session  = (chunkProducer (liftIO pull) "" chunk_producer_helper) $= inputToFrames
    output_to_session = framesToOutput =$ (outputConsumer (liftIO . push))

  in session_start $ do
    (session_sink, session_source) <- session
    liftIO $ forkIO $ session_start $ (input_to_session $$ session_sink)
    -- This thread itself will take care of the outputs...
    liftIO $ putStrLn "Taking care of the outputs"
    liftIO $ session_start $ (session_source $$ output_to_session) 
    return ()







          


	