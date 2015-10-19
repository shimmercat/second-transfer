{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.MainLoop.PushPullType (
   -- | Functions for passing data to external parties
   --   The callbacks here should have a blocking behavior and not
   --   return empty results unless at end of file.

    PushAction
    ,PullAction
    ,BestEffortPullAction
    ,Attendant
    ,CloseAction
    ,IOCallbacks(..)

    ,pushAction_IOC
    ,pullAction_IOC
    ,closeAction_IOC
    ,bestEffortPullAction_IOC

    -- $ classifiers
    ,IOChannels         (..)
    ,PlainTextIO
    ,TLSEncryptedIO
    ,TLSServerIO
    ,TLSClientIO
    ) where


import Control.Lens

import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB

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
    --   protocoll itself.
    _pullAction_IOC               :: PullAction,
    -- | pull data from the channel, as much as the TCP stack wants to provide.
    --   we have no option but use this one when talking HTTP/1.1, where the best
    --   way to know the length is to scan until a Content-Length is found.
    _bestEffortPullAction_IOC     :: BestEffortPullAction,
    -- | this is called when we wish to close the channel.
    _closeAction_IOC              :: CloseAction
    }

makeLenses ''IOCallbacks

-- $classifiers
-- Sometimes we need to classify the IO callbacks according to the operations
-- that they support, so we also create the following classes.

class IOChannels a where
    getIOCallbacks :: a -> IOCallbacks

-- | Data exchanged through this channel is plain text
class IOChannels a => PlainTextIO a
-- | Data exchanges through this channel is the data of a TLS session
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
    getIOCallbacks (PlainText io) = io
instance PlainTextIO PlainText

newtype TLSServer = TLSServer IOCallbacks
instance IOChannels TLSServer where
    getIOCallbacks (TLSServer io) = io
instance TLSEncryptedIO TLSServer
instance TLSServerIO TLSServer

-- | This is an intermediate type. It represents what you obtain
--   by combining something that speaks the protocol and an AwareWorker.
--   In turn, you need to feed a bundle of callbacks implementing I/O
--   to finally start a server.
--
--   You can implement one of these to let somebody else  supply the
--   push, pull and close callbacks. For example, 'tlsServeWithALPN' will
--   supply these arguments to an 'Attendant'.
--
--   Attendants encapsulate all the session book-keeping functionality,
--   which for HTTP/2 is quite complicated. You use the functions
--   http**Attendant
--   to create one of these from a 'SecondTransfer.Types.CoherentWorker'.
--
--   This library supplies two of such Attendant factories,
--   'SecondTransfer.Http1.http11Attendant' for
--   HTTP 1.1 sessions, and 'SecondTransfer.Http2.http2Attendant' for HTTP/2 sessions.
type Attendant = IOCallbacks -> IO ()
