module SecondTransfer.MainLoop (
	-- * Callback types
	Attendant
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


import           SecondTransfer.MainLoop.PushPullType   (Attendant, PullAction,
                                                         PushAction, CloseAction)

import           SecondTransfer.MainLoop.OpenSSL_TLS