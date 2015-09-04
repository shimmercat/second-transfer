{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, OverloadedStrings #-}
module SecondTransfer.Sessions.Internal where

import SecondTransfer.Sessions.Config


import           Control.Concurrent.MVar (MVar, newMVar,modifyMVar)
-- import           Control.Exception       (SomeException)
import qualified Control.Exception       as E
import           Control.Lens            ((^.), makeLenses, Lens' )


import            System.Log.Logger



-- | Contains information that applies to all
--   sessions created in the program. Use the lenses
--   interface to access members of this struct.
--
-- TODO: members of this record should be renamed to the "suffix" convention.
data SessionsContext = SessionsContext {
    -- | Read-only configuration information passed-in at construction time
     _sessionsConfig  :: SessionsConfig
    -- | MVar with enumerator for sessions
    ,_nextSessionId   :: MVar Int
    }


makeLenses ''SessionsContext


sessionsConfig_Sctx :: Lens' SessionsContext SessionsConfig
sessionsConfig_Sctx = sessionsConfig


nextSessionId_Sctx ::  Lens' SessionsContext (MVar Int)
nextSessionId_Sctx = nextSessionId


-- Session tags are simple session identifiers
acquireNewSessionTag :: SessionsContext -> IO Int
acquireNewSessionTag sessions_context =
    modifyMVar
        (sessions_context ^. nextSessionId )
        (\ next_id -> return (next_id+1, next_id))


-- Adds runtime data to a context, and let it work....
makeSessionsContext :: SessionsConfig -> IO SessionsContext
makeSessionsContext sessions_config = do
    next_session_id_mvar <- newMVar 1
    return  SessionsContext {
        _sessionsConfig = sessions_config,
        _nextSessionId = next_session_id_mvar
        }


makeDefaultSessionsContext :: IO SessionsContext
makeDefaultSessionsContext = makeSessionsContext defaultSessionsConfig


sessionExceptionHandler ::
    E.Exception e => SessionComponent -> Int -> SessionsContext -> e -> IO ()
sessionExceptionHandler session_component session_id sessions_context e =
    let

        getit = ( sessionsConfig . sessionsCallbacks . reportErrorCallback_SC )
        maybe_error_callback = sessions_context ^. getit
        component_tag = "Session." ++ show session_component
        error_tuple = (
            session_component,
            SessionCoordinates session_id,
            E.toException e
            )
    in case maybe_error_callback of
        Nothing ->
            errorM component_tag (show e)

        Just callback ->
            callback error_tuple
