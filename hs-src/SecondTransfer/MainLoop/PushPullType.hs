{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
{-# OPTIONS_HADDOCK hide #-}
module SecondTransfer.MainLoop.PushPullType (
	PushAction
	,PullAction
	,Attendant
    ,CloseAction
    ,IOProblem(..)
    ,GenericIOProblem(..)
	) where 


import           Control.Exception 
import           Data.Typeable                (Typeable, cast)

import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB

-- | Callback type to push data to a channel. Part of this 
--   interface is the abstract exception type IOProblem. Throw an 
--   instance of it from here to notify the session that the connection has 
--   been broken. There is no way to signal "normal termination", since 
--   HTTP/2's normal termination can be observed at a higher level when a 
--   GO_AWAY frame is seen. 
type PushAction  = LB.ByteString -> IO ()

-- | Callback type to pull data from a channel. The same
--   as to PushAction applies to exceptions thrown from 
--   there. 
type PullAction  = IO  B.ByteString

-- | Callback that the session calls to realease resources 
--   associated with the channels. Take into account that your
--   callback should be able to deal with non-clean shutdowns
--   also, for example, if the connection to the remote peer
--   is severed suddenly.
type CloseAction = IO ()

-- | A function which takes three arguments: the first one says 
--   how to send data (on a socket or similar transport), and the second one how 
--   to receive data on said socket. The third argument encapsulates 
--   the sequence of steps needed for a clean shutdown. 
--
--   You can implement one of these to let somebody else  supply the 
--   push, pull and close callbacks. In this library we supply callbacks
--   for TLS sockets, so that you don't need to go through the drudgery 
--   of managing those yourself.
--
--   Attendants encapsulate all the session book-keeping functionality,
--   which for HTTP/2 is quite complicated. You use the function `http2Attendant`
--   to create one of these from a `CoherentWorker`.
type Attendant = PushAction -> PullAction -> CloseAction -> IO () 


-- | Throw exceptions derived from this (e.g, `GenericIOProblem` below)
--   to have the HTTP/2 session to terminate gracefully. 
data IOProblem = forall e . Exception e => IOProblem e 
	deriving Typeable


instance  Show IOProblem where
	show (IOProblem e) = show e 

instance Exception IOProblem 

-- | A concrete case of the above exception. Throw one of this
--   if you don't want to implement your own type. Use 
--   `IOProblem` in catch signatures.
data GenericIOProblem = GenericIOProblem
	deriving (Show, Typeable)


instance Exception GenericIOProblem where 
	toException = toException . IOProblem
	fromException x = do 
		IOProblem a <- fromException x 
		cast a