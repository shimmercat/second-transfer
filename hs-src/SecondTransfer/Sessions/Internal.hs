{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings #-}
module SecondTransfer.Sessions.Internal where

import SecondTransfer.Sessions.Config
import SecondTransfer.IOCallbacks.Types  (ConnectionId)


import           Control.Concurrent.MVar (MVar, newMVar,modifyMVar)
import qualified Control.Exception       as E
import           Control.Lens            ((^.), makeLenses, Lens' )



-- | Contains information that applies to all sessions created in the program.
--  Use the lenses interface to access members of this struct.
--
-- TODO: members of this record should be renamed to the "suffix" convention.
data SessionsContext = SessionsContext {
    -- | Read-only configuration information passed-in at construction time
     _sessionsConfig  :: SessionsConfig
    }


makeLenses ''SessionsContext


sessionsConfig_Sctx :: Lens' SessionsContext SessionsConfig
sessionsConfig_Sctx = sessionsConfig


-- Adds runtime data to a context, and let it work....
makeSessionsContext :: SessionsConfig -> IO SessionsContext
makeSessionsContext sessions_config = do
    next_session_id_mvar <- newMVar 1
    return  SessionsContext {
        _sessionsConfig = sessions_config
        }


makeDefaultSessionsContext :: IO SessionsContext
makeDefaultSessionsContext = makeSessionsContext defaultSessionsConfig

sessionExceptionHandler ::
    E.Exception e => SessionComponent -> ConnectionId -> SessionsContext -> e -> IO ()
sessionExceptionHandler session_component session_id sessions_context e =
    let

        getit = ( sessionsConfig . sessionsCallbacks . reportErrorCallback_SC )
        maybe_error_callback = sessions_context ^. getit
        --component_tag = "Session." ++ show session_component
        error_tuple = (
            session_component,
            SessionCoordinates session_id,
            E.toException e
            )
    in case maybe_error_callback of
        Nothing ->
            -- errorM component_tag (show e)
            -- When no callback, ignore the exception
            return ()

        Just callback ->
            callback error_tuple
