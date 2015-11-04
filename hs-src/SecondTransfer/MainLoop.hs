module SecondTransfer.MainLoop (
     _nonce

#ifndef DISABLE_OPENSSL_TLS
    -- * High level OpenSSL functions.
    --
    -- | Use these functions to create your TLS-compliant
    --   HTTP/2 server in a snap.
    , tlsServeWithALPN
#endif



    ) where

import SecondTransfer.TLS.CoreServer

_nonce :: ()
_nonce = undefined
