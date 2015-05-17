module SecondTransfer.Http2(
	BadPrefaceException
	,http2Attendant
	,makeSessionsContext
	,defaultSessionsConfig
	-- | Configuration information
	,SessionsConfig(..)
	-- | Context. You need to use `makeSessionsContext` to instance
	--   one of this. 
	,SessionsContext
	,SessionsCallbacks
	,ErrorCallback
	,sessionsCallbacks

	,reportErrorCallback
	-- | Lens to access the error callback.
	) where

import SecondTransfer.Http2.MakeAttendant(http2Attendant)
import SecondTransfer.Http2.Framer(BadPrefaceException)
import SecondTransfer.Http2.Session(
	defaultSessionsConfig,
	makeSessionsContext,

	SessionsConfig(..),
	SessionsContext,
	SessionsCallbacks,
	ErrorCallback,
	sessionsCallbacks,
	reportErrorCallback
	)