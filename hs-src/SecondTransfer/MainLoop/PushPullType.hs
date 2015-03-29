
module Rede.MainLoop.PushPullType (
	PushAction
	,PullAction
	,Attendant
    ,CloseAction
	) where 


import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB


type PushAction  = LB.ByteString -> IO ()

type PullAction  = IO  B.ByteString

type CloseAction = IO ()

-- | A function which takes two arguments: the first one says 
--   how to send data (on a socket), and the second one how 
--   to receive data on said socket.
type Attendant = PushAction -> PullAction -> CloseAction -> IO () 