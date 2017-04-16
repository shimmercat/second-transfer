{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving, GADTs  #-}
{--

A Last Resort Source Indication socket is one that can use the so called
LARSI protocol for indicating a source address. The protocol is a simple
prefix inserted on the input stream by a remote TCP proxy giving the
address of the original host.

LARSI<flags+length address_ascii><address_ascii>....

The byte following the five "LARSI" ascii value contains both the lenth
of the address in the 6 lower bits (thus up to 64 bytes long) and two
bits of flags.

The two bits of flags should be zero to indicate the current version of
the protocol.

Notice that LARSI must be enabled by the server, otherwise it constitutes a security
vulnerability.

Also, LARSI is optional, even when enabled: a connecting peer may or may not
include a LARSI prefix.

-}
module SecondTransfer.IOCallbacks.WrapLARSISocket (
                 larsiSocketIOCallbacks
               , LARSISocketIOCallbacks
     ) where


import           Control.Monad                                      (unless)
import           Control.Concurrent
import qualified Control.Exception                                  as E
import           Control.Lens                                       (makeLenses, (^.))


import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Builder                            as Bu
import           Data.IORef
import qualified Data.ByteString.Lazy                               as LB
import           Data.ByteString.Char8                              (pack, unpack)
--import           Data.List                                          (find)

import qualified Network.Socket                                     as NS
import qualified Network.Socket.ByteString                          as NSB

import qualified Data.Attoparsec.ByteString                         as ATO
import qualified Data.Attoparsec.ByteString.Char8                   as ATO8


import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.Exception



-- | IOCallbacks around an active LARSI socket
data LARSISocketIOCallbacks = LARSISocketIOCallbacks {
    _socket_LS          :: NS.Socket
  , _prefixReading_LS   :: IORef B.ByteString
  , _originalAddress_LS :: IORef NS.SockAddr
  , _callbacks_LS       :: IOCallbacks
    }


type AcceptResult = Either AcceptErrorCondition LARSISocketIOCallbacks


makeLenses ''LARSISocketIOCallbacks


instance IOChannels LARSISocketIOCallbacks where
    handshake s =
        do
            -- First things first, check if there is a LARSI prefix, by reading
            -- as many bytes as needed to assert that.
            (leftovers_bu, parse_result_bs) <- lookup_prefix (ATO.Partial $ ATO.parse larsi) ""
            let
                -- parse_result_bs = LB.toStrict . Bu.toLazyByteString $ parse_result_bu
                leftovers_bs = LB.toStrict . Bu.toLazyByteString $ leftovers_bu
            writeIORef (s ^. prefixReading_LS) leftovers_bs
            if (B.length parse_result_bs > 0)
              then do
                -- In some circumstances the address may not be
                -- "resolved" correctly
                addr : _ <-
                  E.catch
                      (do
                           addr_infos <-  NS.getAddrInfo
                                (Just $ NS.defaultHints
                                     { NS.addrFlags=[NS.AI_NUMERICHOST], NS.addrSocketType = NS.Stream} )
                                (Just . unpack $ parse_result_bs)
                                Nothing
                           return . map NS.addrAddress  $ addr_infos
                      )
                      ((\ e ->
                           do
                               addr <- NS.getPeerName $ s ^. socket_LS
                               putStrLn "LARSI: parsed address from prefix coulnd't be resolved"
                               return [addr]
                      )::E.IOException -> IO [NS.SockAddr])
                writeIORef (s ^. originalAddress_LS) addr
              else do
                addr <- NS.getPeerName $ s ^. socket_LS
                writeIORef (s ^. originalAddress_LS) addr
            return ( s ^. callbacks_LS )
      where
        lookup_prefix :: ATO.IResult B.ByteString B.ByteString -> Bu.Builder -> IO (Bu.Builder, B.ByteString)
        lookup_prefix (ATO.Partial cont) consumed = do
            datum <- read (s ^. socket_LS)
            case cont datum of
                ATO.Done leftovers larsi_prefix -> return (Bu.byteString leftovers, larsi_prefix)
                a@(ATO.Partial cont') -> lookup_prefix a (consumed `mappend` Bu.byteString datum)
                ATO.Fail _ _ _ -> return (consumed `mappend` Bu.byteString datum, "")
        read socket = do
            datum <- E.catch (NSB.recv socket 4096) uhandler
            if B.length datum == 0
                then do
                   -- Pre-emptively close the socket, don't wait for anything else
                   NS.close (s ^. socket_LS)
                   E.throwIO NoMoreDataException
                else do
                   return  datum

        uhandler :: E.IOException -> IO a
        uhandler = ((\ _e -> do
                               -- Preserve sockets!!
                               -- We can safely close the socket here because
                               -- the close action hasn't  been made available to
                               -- the library's client.
                               NS.close (s ^. socket_LS)
                               E.throwIO NoMoreDataException
                    ) :: E.IOException -> IO a )

        larsi =  do
            ATO8.string "LARSI"
            lng <- ATO.anyWord8
            prefix <- ATO.take (fromIntegral lng)
            return prefix


instance PlainTextIO LARSISocketIOCallbacks

instance HasSocketPeer LARSISocketIOCallbacks where
    getSocketPeerAddress s =  readIORef $ s ^. originalAddress_LS


-- | This function wraps an active socket (e.g., one where it is possible to send and receive data)
--   in something with a set of active callbacks
larsiSocketIOCallbacks :: NS.Socket -> IO LARSISocketIOCallbacks
larsiSocketIOCallbacks socket = do
    socket_already_closed <- newMVar False
    prefix <- newIORef ""
    original_address <- newIORef (error "NOTSetYet")
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
            prefix_s <- readIORef prefix
            if B.length prefix_s > 0
              then do
                writeIORef prefix ""
                return . LB.fromStrict $ prefix_s
              else do
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
    return $ LARSISocketIOCallbacks {
        _socket_LS = socket
      , _callbacks_LS = io_callbacks
      , _prefixReading_LS = prefix
      , _originalAddress_LS = original_address
        }
