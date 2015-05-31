module SecondTransfer.Utils.ConcatConduit(
    concatConduit
    ) where 

#ifndef IMPLICIT_MONOID
import Data.Monoid(mappend, mempty)
#endif
import Data.Conduit 
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as Bu 

concatConduit :: (Monad m) => Sink B.ByteString m B.ByteString
concatConduit = 
    innerConcat mempty 
  where 
    innerConcat :: (Monad m) => Bu.Builder -> Sink B.ByteString m B.ByteString
    innerConcat s = do 
        v <- await 
        case v of 
            Just bs -> 
                innerConcat $ s `mappend` (Bu.byteString bs)

            Nothing -> 
                return $ LB.toStrict . Bu.toLazyByteString $ s
