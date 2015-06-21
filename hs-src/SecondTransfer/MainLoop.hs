module SecondTransfer.MainLoop (

#ifndef DISABLE_OPENSSL_TLS    
    -- * High level OpenSSL functions. 
    -- 
    -- | Use these functions to create your TLS-compliant 
    --   HTTP/2 server in a snap.
    tlsServeWithALPN
    ,tlsServeWithALPNAndFinishOnRequest
#endif

#ifndef DISABLE_OPENSSL_TLS
    ,
#endif

    enableConsoleLogging

#ifndef DISABLE_OPENSSL_TLS
    ,TLSLayerGenericProblem(..)
    ,FinishRequest(..)
#endif

    ) where 


-- We don't need this module in the test suite, and stackage doesn't have a good 
-- enough OpenSSL library yet
#ifndef DISABLE_OPENSSL_TLS
import           SecondTransfer.MainLoop.OpenSSL_TLS
#endif
import           SecondTransfer.MainLoop.Logging         (enableConsoleLogging)