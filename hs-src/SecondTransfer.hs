module SecondTransfer(

    -- * Types related to coherent workers
    --
    -- | A coherent worker is an abstraction that can dance at the 
    --   tune of  HTTP/2. That is, it should be able to take
    --   headers request first, and then a source of data coming in the 
    --   request (for example, POST data). Even before exhausting the source, 
    --   the coherent worker can post the response headers, and then create 
    --   its source for the response data. A coherent worker can also present
    --   early on streams to push. 
	  Headers
    , Request
    , Footers
    , CoherentWorker
    , PrincipalStream
    , PushedStreams
    , PushedStream
    , DataAndConclussion
    , InputDataStream
    , FinalizationHeaders
    
    -- * Basic utilities for TLS HTTP/2 servers
    ,Attendant
    ,PullAction
    ,PushAction
    ,CloseAction
    -- * High level OpenSSL functions. 
    -- 
    -- | Use these functions to create your TLS-compliant 
    --   HTTP/2 server in a snap.
    ,tlsServeWithALPN
    ,tlsServeWithALPNAndFinishOnRequest

    ,TLSLayerGenericProblem(..)
    ,FinishRequest(..)
	) where 

import SecondTransfer.MainLoop.CoherentWorker 
import SecondTransfer.MainLoop