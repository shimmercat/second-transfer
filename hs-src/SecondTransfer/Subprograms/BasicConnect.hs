module Rede.Subprograms.BasicConnect(basicConnectProgram) where 


import qualified  System.Clock as SC
import Rede.Utils
import Network.Simple.TCP (connect)


basicConnectProgram :: String   -- Host name
	-> Int                      -- Port number
	-> IO ()                    -- Time needed to open the connection
basicConnectProgram host port = 
  do
  	base_time <- SC.getTime SC.Monotonic
  	connect host (show port) $ \(_,_) ->
  	  do 
  	  	reportTimedEvent base_time "TCP/connected"
