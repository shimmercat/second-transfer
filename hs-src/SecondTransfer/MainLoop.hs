module SecondTransfer.MainLoop (
    -- * High level OpenSSL functions. 
    -- 
    -- | Use these functions to create your TLS-compliant 
    --   HTTP/2 server in a snap.
    tlsServeWithALPN
    ,tlsServeWithALPNAndFinishOnRequest

    ,enableConsoleLogging

    ,TLSLayerGenericProblem(..)
    ,FinishRequest(..)
    ) where 


import           SecondTransfer.MainLoop.OpenSSL_TLS
import           SecondTransfer.MainLoop.Logging         (enableConsoleLogging)