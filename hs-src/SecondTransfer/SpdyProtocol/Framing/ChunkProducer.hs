module Rede.SpdyProtocol.Framing.ChunkProducer(chunkProducerHelper) where 


import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import           Rede.SpdyProtocol.Framing.AnyFrame ( lengthFromPerfunct
                                           , perfunctoryClassify )



chunkProducerHelper :: Monad m => LB.ByteString 
                       -> m B.ByteString                    -- Generator
                       -> Maybe Int                         -- Length to read
                       -> m (LB.ByteString, LB.ByteString)  -- To yield, left-overs...
chunkProducerHelper pieces gen Nothing = 
    let 
        lazy = pieces 
        length_lazy = LB.length lazy
        perfunctory_classif = perfunctoryClassify lazy
        total_length = lengthFromPerfunct perfunctory_classif
    in if length_lazy >= 8 then
          chunkProducerHelper pieces gen (Just total_length)
       else do 
          new_piece <- gen 
          new_lazy <- return $ LB.append lazy (LB.fromChunks [new_piece]) 
          chunkProducerHelper new_lazy gen Nothing 
chunkProducerHelper lazy gen j@(Just length_to_read) =
    let 
        length_lazy = LB.length lazy
        l64 = fromIntegral length_to_read
    in if length_lazy >= l64 then
            return $ LB.splitAt l64 lazy
        else do 
            new_piece <- gen 
            new_lazy <- return $ LB.append lazy (LB.fromChunks [new_piece]) 
            chunkProducerHelper new_lazy gen j