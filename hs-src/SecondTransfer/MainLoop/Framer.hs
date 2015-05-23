{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.MainLoop.Framer(
    readNextChunk
    ,readNextChunkAndContinue
    ,readLength

    ,Framer
    ,LengthCallback
    ) where


import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class    (MonadIO
                                           -- , liftIO
                                           )
import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as Bu
import qualified Data.ByteString.Lazy      as LB
import           Data.Conduit

-- import           Data.Monoid               (mappend, mempty)


type Framer m =        LB.ByteString                        -- Input left overs
                       -> m B.ByteString                    -- Generator
                       -> Maybe Int                         -- Length to read, if we know now
                       -> m (LB.ByteString, LB.ByteString)  -- To yield, left-overs...



-- * Doing it by parts

type LengthCallback = B.ByteString -> Maybe Int


readNextChunk :: Monad m =>
    LengthCallback                         -- ^ How to know if we can split somewhere
    -> B.ByteString                        -- ^ Input left-overs
    -> m B.ByteString                      -- ^ Generator action
    -> Source m B.ByteString               -- ^ Packet and leftovers, if we could get them 
readNextChunk length_callback input_leftovers gen = do 
    let 

        maybe_length = length_callback input_leftovers

    case maybe_length of 
        Just the_length -> do 
            -- Just need to read the rest .... 
            (package_bytes, newnewleftovers) <- lift $ readUpTo gen input_leftovers the_length
            yield package_bytes 
            readNextChunk length_callback newnewleftovers gen 

        Nothing -> do 
            -- Read a bit more 
            new_fragment <- lift gen 
            let new_leftovers = input_leftovers `mappend` new_fragment
            readNextChunk length_callback new_leftovers gen


-- 
readNextChunkAndContinue :: Monad m =>
    LengthCallback                         -- ^ How to know if we can split somewhere
    -> B.ByteString                        -- ^ Input left-overs
    -> m B.ByteString                      -- ^ Generator action
    -> m (B.ByteString, B.ByteString)      -- ^ Packet bytes and left-overs.
readNextChunkAndContinue length_callback input_leftovers gen = do 
    let 
        maybe_length = length_callback input_leftovers

    case maybe_length of 

        Just the_length -> do 
            -- Just need to read the rest .... 
            (package_bytes, newnewleftovers) <- readUpTo gen input_leftovers the_length
            return (package_bytes, newnewleftovers)

        Nothing -> do 
            -- Read a bit more 
            new_fragment <- gen 
            let new_leftovers = input_leftovers `mappend` new_fragment
            readNextChunkAndContinue length_callback new_leftovers gen


readUpTo :: Monad m => m B.ByteString -> B.ByteString -> Int -> m (B.ByteString, B.ByteString)
readUpTo gen input_leftovers the_length = 
  let 
    initial_length = B.length input_leftovers
    bu = Bu.byteString input_leftovers
    go lo readsofar_length 
        | readsofar_length >= the_length = 
            return $ B.splitAt the_length $ LB.toStrict . Bu.toLazyByteString $ lo
        | otherwise = do
            frag <- gen 
            go (lo `mappend` (Bu.byteString frag)) (readsofar_length + (B.length frag))
  in 
    go bu initial_length

-- Some protocols, e.g., http/2, have the client transmit a fixed-length
-- prefix. This function reads both that prefix and returns whatever get's
-- trapped up there.... 
readLength :: MonadIO m => Int -> m B.ByteString -> m (B.ByteString, B.ByteString)
readLength the_length gen = 
    readUpTo_ mempty 
  where 
    readUpTo_ lo  
      | (B.length lo) >= the_length  = do
            -- liftIO $ putStrLn "Full read"
            return $ B.splitAt the_length lo
      | otherwise = do 
            -- liftIO $ putStrLn $ "fragment read " ++ (show lo) 
            frag <- gen 
            readUpTo_ (lo `mappend` frag)