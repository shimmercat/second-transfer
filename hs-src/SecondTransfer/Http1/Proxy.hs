{-# LANGUAGE OverloadedStrings, TemplateHaskell, FunctionalDependencies #-}
module SecondTransfer.Http1.Proxy (
        ) where

import           Control.Lens
import qualified Data.ByteString                                           as B
import           Data.Conduit

import           SecondTransfer.MainLoop.CoherentWorker                    (Headers)

import           SecondTransfer.Http1.Types
import           SecondTransfer.IOCallbacks.Types



newtype IOCallbacksConn = IOCallbacksConn IOCallbacks


instance Http1CycleController IO IOCallbacksConn where
    releaseResponseResources (IOCallbacksConn conn) = (conn ^. closeAction_IOC)


ioProxyToConnection :: IOCallbacksConn -> HttpRequest IO -> m (HttpResponse IO, IOCallbacksConn)
ioProxyToConnection (IOCallbacksConn ioc) request =
  do

