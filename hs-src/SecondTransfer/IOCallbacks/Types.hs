{-# LANGUAGE ExistentialQuantification, TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module SecondTransfer.IOCallbacks.Types (
               -- | Functions for passing data to external parties
               --   The callbacks here should have a blocking behavior and not
               --   return empty results unless at end of file.

               -- * Fundamental types and accessors
                 PushAction
               , PullAction
               , BestEffortPullAction
               , Attendant
               , CloseAction
               , IOCallbacks        (..)

               , pushAction_IOC
               , pullAction_IOC
               , closeAction_IOC
               , bestEffortPullAction_IOC
               , closeActionCalled_IOC

                 -- * Classifying IO callbacks
                 -- $ classifiers
               , IOChannels         (..)
               , PlainTextIO
               , TLSEncryptedIO
               , TLSServerIO
               , TLSClientIO
               , SOCKS5Preface
               , ConnectionData     (..)
               , addr_CnD
               , connId_CnD
               , nullConnectionData

               , ConnectionId       (..)

               -- * Utility functions
               , PullActionWrapping
               , newPullActionWrapping
               , pullFromWrapping'
               , bestEffortPullFromWrapping
       ) where


import           Control.Lens
import           Control.Concurrent.MVar



import           Data.Int                                     (Int64)

import qualified Data.ByteString.Lazy                         as LB
--import qualified Data.ByteString.Builder                      as Bu

import           SecondTransfer.Sessions.HashableSockAddr     (HashableSockAddr)

-- | Callback type to push data to a channel. Part of this
--   interface is the abstract exception type IOProblem. Throw an
--   instance of it from here to notify the session that the connection has
--   been broken. There is no way to signal "normal termination", since
--   HTTP/2's normal termination can be observed at a higher level when a
--   GO_AWAY frame is seen.
type PushAction  = LB.ByteString -> IO ()

-- | Callback type to pull data from a channel in a best-effort basis.
--   When the first argument is True, the data-providing backend can
--   block if the input buffers are empty and await for new data.
--   Otherwise, it will return immediately with an empty ByteString
type BestEffortPullAction = Bool -> IO LB.ByteString

-- | Callback type to pull data from a channel. The same
--   as to PushAction applies to exceptions thrown from
--   there. The first argument is the number of bytes to
--   pull from the medium. Barring exceptions, we always
--   know how many bytes we are expecting with HTTP/2.
type PullAction  = Int -> IO LB.ByteString

-- | Generic implementation of PullAction from BestEffortPullAction, where we keep around
--   any leftovers data ...
newtype PullActionWrapping = PullActionWrapping (MVar LB.ByteString, BestEffortPullAction)

-- The type above contains stuff already read, its length and the the action

newPullActionWrapping :: BestEffortPullAction -> IO PullActionWrapping
newPullActionWrapping best_effort_pull_action = do
    bu_ref <- newMVar mempty
    return $ PullActionWrapping (bu_ref, best_effort_pull_action)


-- | The type of this function is also PullActionWrapping -> PullAction
--   There should be only one reader concurrently.
pullFromWrapping' :: PullActionWrapping -> Int -> IO LB.ByteString
pullFromWrapping' (PullActionWrapping (mvar_rest, bepa)) n = do
    modifyMVar mvar_rest $ \ hathz ->
        let
            hathz_length = fromIntegral $ LB.length hathz

            go_fetching :: Int -> IO (LB.ByteString, LB.ByteString)
            go_fetching nn = do
                got <- bepa True
                let
                    got_length =  fromIntegral $ LB.length got
                if got_length >= nn
                  then return . LB.splitAt (fromIntegral nn) $ got
                  else do
                    (fragment, rest) <- go_fetching (nn - got_length)
                    return (got `mappend` fragment, rest)

            (u, new_rest) = LB.splitAt (fromIntegral n) hathz

            need_to_fetch_n = n - hathz_length

            if_too_short = do
                (needed_fragment, nr) <- go_fetching need_to_fetch_n
                return (nr, hathz `mappend` needed_fragment)
        in
            if n <= hathz_length
              then
                return (new_rest, u)
              else
                if_too_short



bestEffortPullFromWrapping :: PullActionWrapping -> Bool -> IO LB.ByteString
bestEffortPullFromWrapping (PullActionWrapping (mvar_rest, bepa)) False = do
    hathz <- modifyMVar mvar_rest $ \ rest -> return (mempty, rest)
    let hathz_length = LB.length hathz
    if hathz_length > 0
      then do
        return  hathz
      else do
        -- At least we ought to try
        bepa False


bestEffortPullFromWrapping (PullActionWrapping (mvar_rest, bepa)) True = do
    hathz <- modifyMVar mvar_rest $ \ rest -> return (mempty, rest)
    let hathz_length =  LB.length hathz
    if hathz_length > 0
      then do
        return  hathz
      else do
        bepa True


-- | Callback that the session calls to realease resources
--   associated with the channels. Take into account that your
--   callback should be able to deal with non-clean shutdowns
--   also, for example, if the connection to the remote peer
--   is severed suddenly.
type CloseAction = IO ()

-- | A set of functions describing how to do I/O in a session.
--  There is one rule for IOCallbacks: only one user of it.
--  That is, only one can write, only one can read concurrently.
--  We don't protect for anything else, so concurrent read and
--  writes would result in terrible things happening.
data IOCallbacks = IOCallbacks {
    -- | put some data in the channel
    _pushAction_IOC               :: PushAction,
    -- | get exactly this much data from the channel. This function can
    --   be used by HTTP/2 since lengths are pretty well built inside the
    --   protocoll itself. An exception of type NoMoreDataException can be
    --   raised from here inside if the channel is closed. This is done to
    --   notify the caller that there is no more data. The alternative
    --   would be to return an empty string, but that looks more hazardous
    --   for the caller.
    _pullAction_IOC               :: PullAction,
    -- | pull data from the channel, as much as the TCP stack wants to provide.
    --   we have no option but use this one when talking HTTP/1.1, where the best
    --   way to know the length is to scan until a Content-Length is found.
    --   This will also raise NoMoreDataException when there is no more data.
    --   Notice that with argument False, this may return an empty string.
    _bestEffortPullAction_IOC     :: BestEffortPullAction,
    -- | this is called when we wish to close the channel.
    _closeAction_IOC              :: CloseAction,
    -- | If closeAction has been called or not. We make it an MVar to be able to avoid
    --   concurrent modificationof this, so that in a multi-threaded progra only one
    --   caller be able to actually close the socket.
    _closeActionCalled_IOC        :: MVar Bool
    }

makeLenses ''IOCallbacks


-- $classifiers
-- Sometimes we need to classify the IO callbacks according to the operations
-- that they support, so we also create the following classes.

-- | An object a which is IOChannels
class IOChannels a where
    -- | This method should only be invoked once for the a instance.
    --   It will block/wait for any handshakes to complete, and only then
    --   return a usable set of callbacks.
    handshake :: a -> IO IOCallbacks

-- | Data exchanged through this channel is plain text
class IOChannels a => PlainTextIO a
-- | Data exchanged through this channel is the data of a TLS session
class IOChannels a => TLSEncryptedIO a
-- | Data exchanged through this channel begins with a SOCKS5 negotiation
class IOChannels a => SOCKS5Preface a
-- | The agent putting and retrieving data in this side of the channel should
--   behave as a TLS server
class TLSEncryptedIO a => TLSServerIO a
-- | The agent putting and retrieving data in this side of the channel should
--   behave as a TLS client
class TLSEncryptedIO a => TLSClientIO a

-- | PlainText wrapper
newtype PlainText = PlainText IOCallbacks
instance IOChannels PlainText where
    handshake (PlainText io) = return io
instance PlainTextIO PlainText

newtype TLSServer = TLSServer IOCallbacks
instance IOChannels TLSServer where
    handshake (TLSServer io) = return io
instance TLSEncryptedIO TLSServer
instance TLSServerIO TLSServer



-- | A connection number
newtype ConnectionId = ConnectionId Int64
    deriving (Eq, Ord,  Enum)

makeLenses ''ConnectionId

instance Show ConnectionId where
    show (ConnectionId c) = "C/" ++ (show c)

-- | Some context  related to a connection
data ConnectionData =  ConnectionData {
    _addr_CnD   :: Maybe HashableSockAddr
  , _connId_CnD :: ConnectionId
    }

nullConnectionData :: ConnectionData
nullConnectionData = ConnectionData {
    _addr_CnD = Nothing
  , _connId_CnD = ConnectionId (-1)
    }

makeLenses ''ConnectionData

-- | An Attendant is an entity that can speak a protocol, given
--   the presented I/O callbacks. It's work is to spawn a set
--   of threads to handle a client's session, and then return to
--   the caller. It shouldn'r busy the calling thread.
type Attendant = ConnectionData -> IOCallbacks -> IO ()
