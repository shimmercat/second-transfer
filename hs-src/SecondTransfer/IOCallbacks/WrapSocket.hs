{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.IOCallbacks.WrapSocket (
                 socketIOCallbacks

               , SocketIOCallbacks
               , socket_SS
     ) where


---import           Control.Concurrent
import qualified Control.Exception                                  as E
import           Control.Lens                                       (makeLenses, (^.))

import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Lazy                               as LB
--import           Data.ByteString.Char8                              (pack, unpack)
--import           Data.List                                          (find)

import qualified Network.Socket                                     as NS
import qualified Network.Socket.ByteString                          as NSB


import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.Exception

#include "instruments.cpphs"

-- | IOCallbacks around an active socket
data SocketIOCallbacks = SocketIOCallbacks {
    _socket_SS    :: NS.Socket
  , _callbacks_SS :: IOCallbacks
    }

makeLenses ''SocketIOCallbacks

instance IOChannels SocketIOCallbacks where
    handshake s = return ( s ^. callbacks_SS )

-- | This function wraps an active socket (e.g., one where it is possible to send and receve data)
--   in something with a set of active callbacks
socketIOCallbacks :: NS.Socket -> IO SocketIOCallbacks
socketIOCallbacks socket = do
    let
        uhandler = ((\ e -> do
                               -- TODO: You may need to comment the putStrLn below
                               -- Preserve sockets!!
                               close_action
                               E.throwIO NoMoreDataException
                    ) :: E.SomeException -> IO a )

        push_action lazy_bs =
            E.catch
                (NSB.sendMany socket . LB.toChunks $ lazy_bs)
                uhandler
        -- Unfortunately we are forced to totally rely on sockets blocking or not nature
        -- TODO:  Check if ignoring the flag here becomes a problem.
        best_effort_pull_action _ = do
            datum <- E.catch (NSB.recv socket 4096) uhandler
            if B.length datum == 0
                then do
                   -- Pre-emptively close the socket, don't wait for anything else
                   close_action
                   E.throwIO NoMoreDataException
                else
                   return datum

        -- Exceptions are close are possible
        close_action = E.catch ( do
            REPORT_EVENT("socket-close-called")
            NS.shutdown socket NS.ShutdownBoth
            REPORT_EVENT("socket-shutdown-executed")
            NS.close socket
            REPORT_EVENT("socket-close-executed")
            return ()
            ) (( \ _ -> return () ) :: E.SomeException -> IO () )
    pull_action_wrapping <- newPullActionWrapping  best_effort_pull_action
    let
        pull_action = pullFromWrapping pull_action_wrapping
        io_callbacks = IOCallbacks {
            _pushAction_IOC           = push_action
          , _pullAction_IOC           = pull_action
          , _bestEffortPullAction_IOC = best_effort_pull_action
          , _closeAction_IOC          = close_action
            }
    return $ SocketIOCallbacks {
        _socket_SS = socket
      , _callbacks_SS = io_callbacks
        }
