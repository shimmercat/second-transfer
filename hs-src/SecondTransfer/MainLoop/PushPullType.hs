{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module SecondTransfer.MainLoop.PushPullType (
	PushAction
	,PullAction
	,Attendant
    ,CloseAction
    ,IO_Problem(..)
    ,GenericIOProblem(..)
	) where 


import           Control.Exception 
import           Data.Typeable                (Typeable, cast)

import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB

-- | Callback type to push data to a channel. Part of this 
--   interface is the abstract exception type IO_Problem. Throw an 
--   instance of it to notify the session that the connection has 
--   been broken. 
type PushAction  = LB.ByteString -> IO ()

-- | Callback type to pull data from a channel. The same
--   as to PushAction applies to exceptions thrown from 
--   there. 
type PullAction  = IO  B.ByteString

-- | Callback that the Session calls to realease resources 
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
type Attendant = PushAction -> PullAction -> CloseAction -> IO () 


-- | Throw exceptions derived from this (e.g, `GenericIOProblem` below)
--   to have the HTTP/2 session to terminate gracefully. 
data IO_Problem = forall e . Exception e => IO_Problem e 
	deriving Typeable


instance  Show IO_Problem where
	show (IO_Problem e) = show e 

instance Exception IO_Problem 

-- | A concrete case of the above exception. Throw one of this
--   if you don't want to implement your own type. Capture one 
--   `IO_Problem` otherwise. 
data GenericIOProblem = GenericIOProblem
	deriving (Show, Typeable)


instance Exception GenericIOProblem where 
	toException = toException . IO_Problem
	fromException x = do 
		IO_Problem a <- fromException x 
		cast a