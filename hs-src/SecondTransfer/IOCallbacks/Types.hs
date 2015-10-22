{-# LANGUAGE ExistentialQuantification, TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}
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
               , IOCallbacks(..)

               , pushAction_IOC
               , pullAction_IOC
               , closeAction_IOC
               , bestEffortPullAction_IOC

                 -- * Classifying IO callbacks
                 -- $ classifiers
               , IOChannels         (..)
               , PlainTextIO
               , TLSEncryptedIO
               , TLSServerIO
               , TLSClientIO

               -- * Utility functions
               , PullActionWrapping
               , newPullActionWrapping
               , pullFromWrapping
       ) where


import           Control.Lens


import           Data.IORef
--import           Data.Typeable

import qualified Data.ByteString                              as B
import qualified Data.ByteString.Lazy                         as LB
import qualified Data.ByteString.Builder                      as Bu

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
type BestEffortPullAction = Bool -> IO B.ByteString

-- | Callback type to pull data from a channel. The same
--   as to PushAction applies to exceptions thrown from
--   there. The first argument is the number of bytes to
--   pull from the medium. Barring exceptions, we always
--   know how many bytes we are expecting with HTTP/2.
type PullAction  = Int -> IO B.ByteString

-- | Generic implementation of PullAction from BestEffortPullAction
newtype PullActionWrapping = PullActionWrapping (IORef (Bu.Builder,Int), BestEffortPullAction)

-- The type above contains stuff already read, its length and the the action

newPullActionWrapping :: BestEffortPullAction -> IO PullActionWrapping
newPullActionWrapping best_effort_pull_action = do
    bu_ref <- newIORef (mempty, 0)
    return $ PullActionWrapping (bu_ref, best_effort_pull_action)

-- | The type of this function is also PullActionWrapping -> PullAction
pullFromWrapping :: PullActionWrapping -> Int -> IO B.ByteString
pullFromWrapping (PullActionWrapping (x, bepa)) n = do
    (hathz, len) <- readIORef x
    let
        nn = fromIntegral n
        pullData hathz' len' =
            if n <= len'
              then do
                  let
                      hathz_lb = Bu.toLazyByteString hathz'
                      to_return = LB.toStrict . LB.take nn $ hathz_lb
                      new_hazth = Bu.lazyByteString . LB.drop nn $ hathz_lb
                  return (to_return, new_hazth, len' - n)
              else do
                  -- Need to read more
                  more <- bepa True
                  -- Append
                  let
                      new_len = len + B.length more
                      new_hazth = hathz' `mappend` Bu.byteString more
                  pullData new_hazth new_len
    (to_return, new_hazth, new_len) <- pullData hathz len
    writeIORef x (new_hazth, new_len)
    return to_return


-- | Callback that the session calls to realease resources
--   associated with the channels. Take into account that your
--   callback should be able to deal with non-clean shutdowns
--   also, for example, if the connection to the remote peer
--   is severed suddenly.
type CloseAction = IO ()

-- | A set of functions describing how to do I/O in a session.
--   As usual, we provide lenses accessors.
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
    _closeAction_IOC              :: CloseAction
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

-- | An Attendant is an entity that can speak a protocol, given 
--   the present I/O callbacks.
type Attendant = IOCallbacks -> IO ()
