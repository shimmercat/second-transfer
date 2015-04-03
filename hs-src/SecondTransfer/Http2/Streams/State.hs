{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
             FlexibleInstances, MultiParamTypeClasses,
             TemplateHaskell
 #-}

{- 

This module plugs a worker (check module ...MainLoop.Tokens, and there type StreamWorker) 
and adapts it so that it speaks in frames. So, it implements translation between the 
generic tokens present at Tokens.hs and the frames. 

-}

module SecondTransfer.Http2.Streams.State(
    initStreamState
    ,inputPlug

    ,StreamStage
    ,StreamStateT
    ) where 


import qualified Blaze.ByteString.Builder            as Bu
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Lens
import qualified Control.Lens                        as L
-- import           Control.Monad.Morph                 (MFunctor)
-- import           Data.BitSet.Generic                 (singleton)
import qualified Data.ByteString                     as B
-- import qualified Data.ByteString.Lazy                as LB
import           Data.Conduit
-- import           Data.Conduit.Lift                   (readerC)
-- import           Data.Default
import qualified Data.HashTable.IO                   as H
import           Data.IORef

import           Data.Monoid                         (mappend, mempty)

-- import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent.Chan 
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader



-- No framing layer here... let's use Kazu's Yamamoto library
import qualified Network.HTTP2            as NH2
import qualified Network.HPACK            as HP

-- import           SecondTransfer.MainLoop.StreamPlug (
--                                           StreamId (..), 
--                                           -- StreamPlug (..)
--                                           )
import           SecondTransfer.MainLoop.CoherentWorker


