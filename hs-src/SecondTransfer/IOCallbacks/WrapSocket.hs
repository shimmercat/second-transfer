{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving, GADTs  #-}
module SecondTransfer.IOCallbacks.WrapSocket (
                 socketIOCallbacks

               , SocketIOCallbacks
               , socket_SS

               , HasSocketPeer                                     (..)
               , SomeHasSocketPeer                                 (..)

               , AcceptErrorCondition                              (..)

               , AcceptResult
     ) where


import           Control.Monad                                      (unless)
import           Control.Concurrent
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


-- | IOCallbacks around an active socket
data SocketIOCallbacks = SocketIOCallbacks {
    _socket_SS    :: NS.Socket
  , _callbacks_SS :: IOCallbacks
    }


type AcceptResult = Either AcceptErrorCondition SocketIOCallbacks


makeLenses ''SocketIOCallbacks

instance IOChannels SocketIOCallbacks where
    handshake s = return ( s ^. callbacks_SS )


instance PlainTextIO SocketIOCallbacks

instance HasSocketPeer SocketIOCallbacks where
    getSocketPeerAddress s = NS.getPeerName $ s ^. socket_SS


-- | This function wraps an active socket (e.g., one where it is possible to send and receive data)
--   in something with a set of active callbacks
socketIOCallbacks :: NS.Socket -> IO SocketIOCallbacks
socketIOCallbacks socket = do
    socket_already_closed <- newMVar False
    let
        uhandler :: E.IOException -> IO a
        uhandler = ((\ _e -> do
                               -- Preserve sockets!!
                               -- putStrLn $ E.displayException _e
                               close_action
                               E.throwIO NoMoreDataException
                    ) :: E.IOException -> IO a )

        -- A socket is closed inmediately upon finding an exception.
        -- The close action will be called many more times, of course,
        -- since the entire program is very, very overzealous of
        -- open sockets.

        -- We, of course, want exceptions to bubble from here.
        push_action lazy_bs = -- keyedReportExceptions "pushAtSocket" $
            E.catch
                (NSB.sendMany socket . LB.toChunks $ lazy_bs)
                uhandler

        best_effort_pull_action _ = do
            datum <- E.catch (NSB.recv socket 4096) uhandler
            if B.length datum == 0
                then do
                   -- Pre-emptively close the socket, don't wait for anything else
                   close_action
                   E.throwIO NoMoreDataException
                else do
                   return . LB.fromStrict $ datum

        -- Exceptions on close are possible
        close_action = modifyMVar_ socket_already_closed $ \ already_closed -> do
            unless (already_closed) $ do
                E.finally
                    (ignoreException ioException () $ NS.shutdown socket NS.ShutdownBoth)
                    (ignoreException ioException () $ NS.close socket)
            return True

    pull_action_wrapping <- newPullActionWrapping  best_effort_pull_action
    let
        pull_action = pullFromWrapping' pull_action_wrapping
        best_effort_pull_action'  = bestEffortPullFromWrapping pull_action_wrapping
        io_callbacks = IOCallbacks {
            _pushAction_IOC           = push_action
          , _pullAction_IOC           = pull_action
          , _bestEffortPullAction_IOC = best_effort_pull_action'
          , _closeAction_IOC          = close_action
          , _closeActionCalled_IOC    = socket_already_closed
            }
    return $ SocketIOCallbacks {
        _socket_SS = socket
      , _callbacks_SS = io_callbacks
        }
