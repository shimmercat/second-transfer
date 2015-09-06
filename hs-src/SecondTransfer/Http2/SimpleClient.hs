{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- TODO: Remove this file, it is not needed!!
module SecondTransfer.Http2.SimpleClient (
    ClientState(..)  -- reduced functionality is exported to the user, but
                     -- the framework may need a lot.
    ,pendingRequests_ClS
                                   ) where


import           Control.Lens
import           Control.Concurrent.Chan
-- System grade utilities
import           Control.Concurrent                     (ThreadId, forkIO)
import           Control.Concurrent.Chan
import           Control.Exception                      (throwTo)
import qualified Control.Exception                      as E
import           Control.Monad                          (forever,unless, when, mapM_, forM, forM_)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.DeepSeq                        ( ($!!), deepseq )
import           Control.Monad.Trans.Reader
-- import           Control.Monad.Catch                    (throwM)
import           Control.Concurrent.MVar


import           Data.Conduit
--import

import           SecondTransfer.MainLoop.ClientPetitioner
import           SecondTransfer.MainLoop.CoherentWorker                       (Headers,InputDataStream)
import           SecondTransfer.Http2.Session                                 (ClientState(..),
                                                                               pendingRequests_ClS
                                                                              )

void::()
void = ()
