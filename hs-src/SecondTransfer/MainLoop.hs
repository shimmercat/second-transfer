module SecondTransfer.MainLoop (
	-- * Callbacks
	--
	-- | These callback make possible to separate the parts in layers. 
	--   The `Attendant` is a function that can push and pull bytes to/from
	--   a transport (for example, a socket), but it is not concerned on how
	--   those bytes are pushed or pulled. 
	Attendant
	,PullAction
	,PushAction
	,CloseAction
	,IOProblem
	,GenericIOProblem
	-- * High level OpenSSL functions. 
	-- 
	-- | Use these functions to create your TLS-compliant 
	--   HTTP/2 server in a snap.
	,tlsServeWithALPN
    ,tlsServeWithALPNAndFinishOnRequest

    ,enableConsoleLogging

    ,TLSLayerGenericProblem(..)
    ,FinishRequest(..)
	) where 


import           SecondTransfer.MainLoop.PushPullType   (Attendant, PullAction,
                                                         PushAction, CloseAction,
                                                         IOProblem, GenericIOProblem
                                                         )

import           SecondTransfer.MainLoop.OpenSSL_TLS
import           SecondTransfer.MainLoop.Logging         (enableConsoleLogging)