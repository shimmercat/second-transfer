{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

{- 

This module plugs a worker (check module ...MainLoop.Tokens, and there type StreamWorker) 
and adapts it so that it speaks in frames. So, it implements translation between the 
generic tokens present at Tokens.hs and the frames. 

-}


module SecondTransfer.SpdyProtocol.Streams.State(
    defaultStreamState
    ,initStreamState
    ,packSendHeaders
    ,streamFinalize
    ,inputPlug
    ,outputPlug

    ,StreamStage(..)
    ,StreamState(..)
    ,StreamStateT(..)
    ) where 


import           Data.IORef
import qualified Data.ByteString           as B
import           Data.Default
import           Control.Monad.Morph       (MFunctor)
import           Data.Conduit
import           Data.BitSet.Generic(singleton)

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Concurrent.MVar
import qualified Data.HashTable.IO          as H


import           SecondTransfer.MainLoop.StreamPlug
import           SecondTransfer.MainLoop.Tokens
import           SecondTransfer.SpdyProtocol.Framing.AnyFrame
import           SecondTransfer.SpdyProtocol.Framing.Frame
import           SecondTransfer.SpdyProtocol.Framing.KeyValueBlock
import           SecondTransfer.SpdyProtocol.Framing.SynReply
import           SecondTransfer.SpdyProtocol.Framing.Headers
import qualified SecondTransfer.SpdyProtocol.Framing.SynStream     as SyS
import           SecondTransfer.SpdyProtocol.Framing.DataFrame
import qualified SecondTransfer.SpdyProtocol.Framing.DataFrame     as DF
import           SecondTransfer.Constants(spdyDataFrameMaxLength)


data StreamStage =
     Open_StS
    |CanOnlySend_StS
    |CanOnlyReceive_StS
    |Closed_StS
    ;


canSend :: StreamStage -> Bool 
canSend Open_StS             = True 
canSend CanOnlySend_StS      = True 
canSend _                    = False


data WindowBookkeeping = WindowBookkeeping {
      pastBytes   :: Int 
    , futureBytes :: Int 
}


instance Default WindowBookkeeping where 
    def = WindowBookkeeping {
        pastBytes = 0
        ,futureBytes = 0
    }


-- A dictionary from local id to (global) stream id
type PushStreamsHashTable = H.BasicHashTable Int Int



-- Due to the use of Z here, this struct only
-- makes sense inside the IO monad (or equivalent)
data StreamState = StreamState {
      stage      :: IORef StreamStage

    -- This main stream Id. This should be an odd number, because
    -- the stream is always started by the browser
    , sstreamId  :: Int

    -- Did I Acknowledged the string already with SynReply?
    , mustAck    :: IORef Bool

    -- What should I do when this stream finishes?
    , finalizer  :: IO ()

    -- A dictionary from local to global id of the streams...
    , pushStreamHashTable :: PushStreamsHashTable  

    -- What should be the id of the next pushed stream?
    , nextPushStreamId :: MVar Int 
    }


newtype MonadIO m => StreamStateT m a = StreamStateT (ReaderT StreamState m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MFunctor)


instance MonadIO m => MonadIO (StreamStateT m) where 
    -- liftIO :: IO a -> m a 
    liftIO iocomp = StreamStateT (liftIO iocomp) 


inputPlug :: Conduit AnyFrame (StreamStateT IO) StreamInputToken
inputPlug = do 
    maybe_anyframe <- await 
    case maybe_anyframe of
        Just anyframe -> do 
            stream_input_token <- lift $ anyFrameToInput anyframe
            yield stream_input_token
            inputPlug
        -- TODO: Close StreamWorker? 
        _             -> return ()


outputPlug :: Conduit StreamOutputAction (StreamStateT IO) AnyFrame 
outputPlug = do 
    maybe_action <- await 
    case maybe_action of 

        -- Stream worker asks to send headers
        Just (SendHeaders_SOA unmvl) -> do 
            -- TODO: MUST check that the stream be openened to 
            --       send, take measure if not!!
            current_stage <- lift getCurrentStreamStage  
            must_ack      <- lift getMustAck 
            case (current_stage, must_ack) of 

                (s, True ) | canSend s -> do
                    any_frame <- lift $ prepareSynReplyFrame unmvl
                    yield any_frame

                (s, False) | canSend s -> do
                    anyframe <- lift $ prepareHeadersFrame unmvl
                    yield anyframe
                -- TODO: 10 000 000 cases more here that need to be populated...
            if must_ack 
                then lift $ setMustAck False 
                else return ()

            outputPlug

        -- Stream worker asks to send data 
        Just (SendData_SOA bs_data)     -> do
            current_stage <- lift getCurrentStreamStage
            must_ack      <- lift getMustAck
            stream_id     <- lift getStreamId
            liftIO $ putStrLn ("Send main stream data of " ++ (show stream_id) )
            liftIO $ putStrLn ("Length: " ++ (show $ B.length bs_data))
            case (current_stage, must_ack) of 
                (s,True) | canSend s -> do 
                    liftIO $ putStrLn "Must ack on data!!"
                    anyframe <- lift $ prepareSynReplyFrame (UnpackedNameValueList [])
                    yield anyframe
                    -- yield $ prepareDataFrame bs_data stream_id
                    yieldDataFramesFromData bs_data stream_id
                    lift $ setMustAck False
                (s,False) | canSend s -> do 
                    --yield $ prepareDataFrame bs_data stream_id
                    yieldDataFramesFromData bs_data stream_id 
            outputPlug

        Just Finish_SOA                 -> do
            current_stage <- lift getCurrentStreamStage
            must_ack      <- lift getMustAck
            stream_id     <- lift getStreamId
            case (current_stage, must_ack) of 

                -- I'm finishing without sending any headers
                (s,True) | canSend s -> do 
                    anyframe <- lift $ prepareSynReplyFrameAndFinish (UnpackedNameValueList [])
                    yield anyframe
                    lift $ setMustAck False

                (s,False) | canSend s -> do 
                    yield $ prepareFinishDataFrame stream_id

                _                     -> do

                    liftIO $ putStrLn "Protocol error: trying to send on closed stream"

            outputPlug

        -- Let's work with associated streams... 
        Just (SendAssociatedHeaders_SOA local_stream_id unmvl) -> do
            -- Get a global id for this dear ... 
            stream_id        <- lift getStreamId
            global_stream_id <- lift $ getGlobalPushStreamId local_stream_id 

            -- Now, I need to open a new stream for this.... 
            anyframe         <- lift $ prepareSynStreamFrame global_stream_id stream_id unmvl
            yield anyframe

            outputPlug


        Just (SendAssociatedData_SOA local_stream_id contents)  -> do 
            -- Global id 
            global_stream_id <- lift $ getGlobalPushStreamId local_stream_id

            -- anyframe         <- return $ prepareDataFrame contents global_stream_id
            -- yield anyframe
            yieldDataFramesFromData contents global_stream_id
            outputPlug


        Just (SendAssociatedFinish_SOA local_stream_id)  -> do 
            global_stream_id  <- lift $ getGlobalPushStreamId local_stream_id
            yield $ prepareFinishDataFrame global_stream_id
            outputPlug


        -- Just something_else             -> do 
        --     liftIO $ putStrLn $ "Got: " ++ (show something_else)
        --     outputPlug

        Nothing                         -> 
            return ()


prepareDataFrame :: B.ByteString -> Int -> AnyFrame
prepareDataFrame bs_data stream_id = DataFrame_AF $ DataFrame {
    DF.streamId        = stream_id
    ,DF.dataFrameFlags = fbs0
    ,DF.payload        = bs_data
    }


yieldDataFramesFromData :: B.ByteString
                 -> Int -> ConduitM StreamOutputAction AnyFrame (StreamStateT IO) ()
yieldDataFramesFromData contents stream_id = do 
    yield $ prepareDataFrame start stream_id
    case rest of 
        ""     -> return ()
        longer -> yieldDataFramesFromData longer stream_id 
  where 
    (start, rest) = B.splitAt spdyDataFrameMaxLength contents


prepareFinishDataFrame :: Int -> AnyFrame
prepareFinishDataFrame stream_id = DataFrame_AF $ DataFrame {
                            DF.streamId        = stream_id
                            ,DF.dataFrameFlags = singleton DF.Fin_F
                            ,DF.payload        = ""
                            }


anyFrameToInput :: MonadIO m => AnyFrame ->  StreamStateT m StreamInputToken
anyFrameToInput any_frame = 

    case any_frame of 

        ( AnyControl_AF (SynStream_ACF synstream) ) -> let 
               UncompressedKeyValueBlock  unvl                       = getCompressedHeaders synstream
               stream_unidirectional                                 = getFrameFlag synstream SyS.Unidirectional_SSVF
               stream_fin                                            = getFrameFlag synstream SyS.Fin_SSVF
            in 
              do 
                case (stream_unidirectional, stream_fin) of 
                    (True, True)       ->  setCurrentStreamStage  Closed_StS
                    (True, False)      ->  setCurrentStreamStage  CanOnlyReceive_StS
                    (False, True)      ->  setCurrentStreamStage  CanOnlySend_StS
                    (False, False)     ->  setCurrentStreamStage  Open_StS
                -- liftIO $ putStrLn "Frame translated"
                return $ Headers_STk unvl

        -- TODO: Why are we not reacting to the data here? because 
        -- so far we don't support uploads
        _  -> 
            error "Not implemented!!"


getGlobalPushStreamId :: MonadIO m => Int -> StreamStateT m Int 
getGlobalPushStreamId local_stream_id = StreamStateT $ do
    ht <- asks pushStreamHashTable

    -- Do I have already an id for this one?
    global_id_maybe <- liftIO $ H.lookup ht local_stream_id
    case global_id_maybe of 

        Just global_id    -> 
            -- Great, return it 
            return global_id

        Nothing -> do
            -- Complicated dance 
            next_pushed_stream_mvar <- asks nextPushStreamId
            next_pushed_stream_id <- liftIO $ modifyMVar next_pushed_stream_mvar $ \ n -> do 
                H.insert ht local_stream_id n
                -- Must keep the pushed streams even....
                return (n+2, n)
            return next_pushed_stream_id


prepareSynReplyFrame :: UnpackedNameValueList -> StreamStateT IO AnyFrame 
prepareSynReplyFrame unmvl = do 
    compressed_headers <- packSendHeaders unmvl
    stream_id          <- getStreamId
    syn_reply          <- return $ SynReplyFrame def stream_id compressed_headers
    return $ wrapCF  syn_reply   


prepareSynStreamFrame :: Int -> Int -> UnpackedNameValueList -> StreamStateT IO AnyFrame 
prepareSynStreamFrame global_stream_id associated_to_stream unmvl = do 
    compressed_headers <- packSendHeaders unmvl 
    syn_stream         <- return $ setFrameFlag (
                                SyS.SynStreamFrame def global_stream_id associated_to_stream compressed_headers 
                                )  SyS.Unidirectional_SSVF
    return $ wrapCF syn_stream


prepareHeadersFrame :: UnpackedNameValueList -> StreamStateT IO AnyFrame 
prepareHeadersFrame unmvl = do 
    compressed_headers <- packSendHeaders unmvl
    stream_id          <- getStreamId
    headers_frame      <- return $ HeadersFrame def stream_id compressed_headers
    return $ wrapCF headers_frame 


prepareSynReplyFrameAndFinish :: UnpackedNameValueList -> StreamStateT IO AnyFrame 
prepareSynReplyFrameAndFinish unmvl = do 
    compressed_headers <- packSendHeaders unmvl
    stream_id          <- getStreamId
    syn_reply          <- return $ SynReplyFrame finish_prologue stream_id compressed_headers
    return (AnyControl_AF (SynReplyFrame_ACF syn_reply))
  where 
    finish_prologue = ControlFrame SynReply_CFT (fbs1 Fin_SRVF) 0   



packSendHeaders :: MonadIO m => UnpackedNameValueList -> StreamStateT m CompressedKeyValueBlock
packSendHeaders uncompressed_uvl = do 
    return $ UncompressedKeyValueBlock uncompressed_uvl


ioSet :: MonadIO m => (StreamState -> IORef a) -> a -> StreamStateT m ()
ioSet member new_val  = StreamStateT $ do 
    io_ref <- asks member
    liftIO $ writeIORef io_ref new_val


ioGet :: MonadIO m => (StreamState -> IORef a) -> StreamStateT m a
ioGet member = StreamStateT $ do 
    io_ref <- asks member
    liftIO $ readIORef io_ref
 

-- Member get/set from the stream state computation monad 
getCurrentStreamStage :: MonadIO m => StreamStateT m  StreamStage
getCurrentStreamStage = ioGet stage

setCurrentStreamStage :: MonadIO m => StreamStage -> StreamStateT m () 
setCurrentStreamStage = ioSet stage

getStreamId :: MonadIO m => StreamStateT m Int 
getStreamId = StreamStateT $ asks sstreamId

setMustAck  :: MonadIO m => Bool -> StreamStateT m ()
setMustAck = ioSet mustAck

getMustAck :: MonadIO m => StreamStateT m Bool 
getMustAck = ioGet mustAck

streamFinalize :: MonadIO m => StreamStateT m ()
streamFinalize = StreamStateT $ do 
    fin <- asks finalizer
    liftIO fin


initStreamState :: MonadIO m => Int -> (IO () ) -> MVar Int -> StreamStateT m a -> m a 
initStreamState stream_id fin next_pushed_stream (StreamStateT sm)  = do 
    s <- liftIO $ defaultStreamState stream_id fin next_pushed_stream
    runReaderT sm s


defaultStreamState :: Int -> (IO () ) ->  MVar Int -> IO StreamState 
defaultStreamState stream_id fin next_push_id = do
    stage_ioref <- newIORef Closed_StS
    ma                     <- newIORef True
    push_stream_hash_table <- H.new  
    return $ StreamState {
        stage                  = stage_ioref
        ,sstreamId             = stream_id
        ,mustAck               = ma
        ,finalizer             = fin
        ,pushStreamHashTable   = push_stream_hash_table
        ,nextPushStreamId      = next_push_id
    }

