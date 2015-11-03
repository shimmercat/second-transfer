{-# LANGUAGE OverloadedStrings, TemplateHaskell, FunctionalDependencies #-}
module SecondTransfer.Http1.Types (
                 HttpRequest                                               (..)
               , headers_Rq
               , body_Rq


               , HttpResponse                                              (..)
               , headers_Rp
               , body_Rp

               , Http1CycleController                                      (..)
               , ProxyToHttpServer                                         (..)


        ) where

import           Control.Lens
import qualified Data.ByteString                                           as B
import           Data.Conduit

import           SecondTransfer.MainLoop.CoherentWorker                    (Headers)

----- Types--- I may move some of the types here later to CoherentWorker --------
-- ... or to some other common-use file.

-- | Request. This is an old-fashioned HTTP request, with less data than that
--   defined at CoherentWorker: just headers and perhaps a request streaming body.
--   As in other places in this library, we expect method and path to be
--   given as pseudo-headers
data HttpRequest m = HttpRequest {
    _headers_Rq         :: Headers
  , _body_Rq            :: Source m B.ByteString
    }

makeLenses ''HttpRequest


-- | Response. Status should be given as a pseudo-header
data HttpResponse m = HttpResponse {
    _headers_Rp         :: Headers
  , _body_Rp            :: Source m B.ByteString
    }

makeLenses ''HttpResponse

-- Used for early release of resources
class Monad m => Http1CycleController m contrl where
    releaseResponseResources :: contrl -> m ()

-- | Something that can talk to a HTTP 1.1 server by using a connection and sending
--   the request to it
class (Http1CycleController m contrl, Monad m) => ProxyToHttpServer m conn contrl | conn -> contrl  m where
    -- Sends the request to server, gets the response
    proxyToConnection ::
          conn
       -> HttpRequest m
       -> m (HttpResponse m, contrl)
