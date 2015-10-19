{-# LANGUAGE ForeignFunctionInterface #-}
module SecondTransfer.TLS.Botan (
       ) where

import           Foreign
import           Foreign.C.Types                                           (CChar)
import qualified Data.ByteString                                           as B
import qualified Data.ByteString.Lazy                                      as LB

import           Control.Lens                                              ( (^.) )

-- Import all of it!
import           SecondTransfer.MainLoop.PushPullType


type SIOCallbacks = StablePtr IOCallbacks

# Some comments
foreign export ccall iocba_push :: SIOCallbacks -> Ptr CChar -> Int -> IO ()
iocba_push siocb p len = do
    io_callbacks <- deRefStablePtr siocb
    let cstr = (p, fromIntegral len)
    b <- B.packCStringLen cstr
    (io_callbacks ^. pushAction_IOC) (LB.fromStrict b)
