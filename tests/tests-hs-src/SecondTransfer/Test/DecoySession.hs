{-# LANGUAGE TemplateHaskell #-}

module SecondTransfer.Test.DecoySession(
	createDecoySession,
	DecoySession
	) where 


import SecondTransfer.Http2

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Lens            (view)
import qualified Control.Lens            as L



data DecoySession = DecoySession {
	_inputDataChannel   :: Chan (Maybe LB.ByteString)
	,_outputDataChannel :: Chan (Maybe B.ByteString),
	,_sessionThread     :: ThreadId
	}


L.makeLenses ''FramerSessionData


-- Supposed to be an HTTP/2 attendant
createDecoySession :: Attendant -> IO DecoySession
createDecoySession attendant = 
	input_data_channel  <- newChan 
	output_data_channel <- newChan
	let 
		push_action :: PushAction
		push_action = writeChan input_data_channel
		pull_action :: PullAction 
		pull_action = readChan output_data_channel
		close_action :: CloseAction 
		close_action = 
			-- TODO: This is a good place to check for 
			--  ramining data on the pipes....
			return ()
	thread_id <- forkIO $ attendant push_action pull_action close_action
	let session = DecoySession {
		_inputDataChannel  = input_data_channel,
		_outputDataChannel = output_data_channel,
		_sessionThread     = thread_id
		}
	return session


-- Tell when we are done with a dession
sessionIsUndone :: DecoySession -> IO Bool
sessionIsUndone session = error "NotImplemented"


-- Send a frame to a session 
sendFrameToSession :: DecoySession -> OutputFrame -> IO ()
sendFrameToSession session (encode_info, frame_payload) = do 
	let 
		bs_list = encodeFrameChunks encode_info frame_payload
		input_data_channel = session ^. inputDataChannel
	mapM_ ( \ s -> writeChan input_data_channel s) bs_list


-- Read a frame from a session... if possible 
recvFrameFromSession :: DecoySession ->  IO (Maybe NH2.Frame)
recvFrameFromSession decoy_session = do
	F.readNextChunk http2FrameLength remaining pull_action


-- Send raw data to a session 
sendRawDataToSession :: DecoySession -> B.ByteString -> IO ()
sendRawDataToSession decoy_session data_to_send = error "NotImplemented"


-- Read raw data from a session. Normally blocks until data be available.
-- It returns Nothing when the session is to be considered finished. 
recvRawDataFromSession :: DecoySession -> IO (Maybe B.ByteString)
recvRawDataFromSession decoy_session = error "NotImplemented"