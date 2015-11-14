{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell, Rank2Types, FunctionalDependencies, OverloadedStrings #-}

-- Module in charge of inserting noise in the middle of some communications, to see how advanced are
-- our error-handling capabilities....
--

module SecondTransfer.IOCallbacks.Botcher (
                 insertNoise
       ) where

import           Control.Concurrent
import qualified Control.Exception                                         as E

import           Data.Tuple                                                (swap)
import           Data.Typeable                                             (Proxy(..))
import           Data.Maybe                                                (fromMaybe)
import           Data.IORef
import qualified Data.ByteString                                           as B
import qualified Data.ByteString.Builder                                   as Bu
import qualified Data.ByteString.Lazy                                      as LB
import qualified Data.ByteString.Unsafe                                    as Un

import           Control.Lens                                              ( (^.), makeLenses,
                                                                             --set, Lens'
                                                                           )
import           Data.IORef

-- Import all of it!
import           SecondTransfer.IOCallbacks.Types


insertNoise :: Int -> B.ByteString -> IOCallbacks -> IO IOCallbacks
insertNoise offset noise (IOCallbacks push pull bpa ca) =
  do
    written_data <- newIORef 0
    recv_data <- newIORef 0
    let
        npush bs = do
            n <- readIORef written_data
            if n > offset
               then
                  push . LB.fromStrict $ noise
               else do
                  atomicModifyIORef' written_data $ \ nn -> (nn + (fromIntegral $ LB.length bs), ())
                  push bs
        npull n = do
            rc <- readIORef recv_data
            if rc > offset
               then
                  return noise
               else do
                  g <- pull n
                  putStrLn . show $ ( B.length g /= n)
                  atomicModifyIORef' recv_data $ \ nn -> (nn + (fromIntegral $ B.length g), ())
                  return g

        nbpa cb = do
            rc <- readIORef recv_data
            if rc > offset
               then
                  return noise
               else do
                  g <- bpa cb
                  atomicModifyIORef' recv_data $ \ nn -> (nn + (fromIntegral $ B.length g), ())
                  return g


    return $ IOCallbacks npush npull nbpa ca
