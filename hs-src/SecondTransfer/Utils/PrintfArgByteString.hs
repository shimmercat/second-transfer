{-# OPTIONS_GHC -fno-warn-orphans #-}
module SecondTransfer.Utils.PrintfArgByteString where 


import           Data.ByteString       hiding (unpack)
import           Data.ByteString.Char8 (unpack)
import           Text.Printf



instance PrintfArg ByteString where 

    formatArg x fmt | fmtChar (vFmt 's' fmt) == 's' =
        formatString (unpack x) (fmt { fmtChar = 's', fmtPrecision = Nothing })
    formatArg _ fmt = errorBadFormat $ fmtChar fmt