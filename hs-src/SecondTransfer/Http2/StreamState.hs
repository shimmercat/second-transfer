module SecondTransfer.Http2.StreamState (
                 openStream
               , closeStreamRemote
               , closeStreamLocal
               , countActiveStreams
               , getStreamState

               , StreamState            (..)
               , StreamStateTable
                                        ) where


import qualified Data.HashTable.IO                      as H
import           Data.Word

type HashTable k v = H.CuckooHashTable k v

type GlobalStreamId = Int

type StreamStateTable = HashTable GlobalStreamId Word8


-- Bits:
--   0 : half-closed (local)
--   1 : half-closed (remote)
--
-- Numbers:
--   0 : open
--   1 : half-closed (local)
--   2 : half-closed (remote)
--   3 -> Delete : stream is closed or idle

data StreamState =
    Open_SS
  | HalfClosedLocal_SS
  | HalfClosedRemote_SS
  | Idle_SS
  | Closed_SS


getStreamState :: StreamStateTable -> GlobalStreamId -> GlobalStreamId -> IO StreamState
getStreamState statetable maxid queryid =
  do
    maybe_state <- H.lookup statetable queryid
    case maybe_state of
        Nothing | maxid >= queryid -> return Closed_SS
                | otherwise -> return Idle_SS
        Just 0 -> return Open_SS
        Just 1 -> return HalfClosedLocal_SS
        Just 2 -> return HalfClosedRemote_SS
        Just _ -> do
             H.delete statetable queryid
             return $ Closed_SS

-- Debugging function
-- report :: StreamStateTable -> String -> IO ()
-- report statetable msg = do
--   active_streams <- countActiveStreams statetable
--   putStrLn $ "ActiveStreams: " ++ msg ++  " " ++  (show active_streams)


openStream :: StreamStateTable  -> GlobalStreamId -> IO ()
openStream statetable stream_id = do
    -- report statetable "before-open"
    H.insert statetable stream_id 0


closeStreamRemote :: StreamStateTable -> GlobalStreamId -> IO ()
closeStreamRemote statetable stream_id = do
    maybe_state <- H.lookup statetable stream_id
    let
        new_value = case maybe_state of
            Nothing -> Nothing
            Just 0 -> Just 2
            Just 1 -> Nothing
            Just 2 -> Just 2
            Just _ -> Nothing

    case new_value of
        Just x -> H.insert statetable stream_id x
        Nothing -> H.delete statetable stream_id

    -- report statetable "after-close-remote"


closeStreamLocal :: StreamStateTable -> GlobalStreamId -> IO ()
closeStreamLocal statetable stream_id = do
    maybe_state <- H.lookup statetable stream_id
    let
        new_value = case maybe_state of
            Nothing -> Nothing
            Just 0 -> Just 1
            Just 1 -> Just 1
            Just 2 -> Nothing
            Just _ -> Nothing

    case new_value of
        Just x -> H.insert statetable stream_id x
        Nothing -> H.delete statetable stream_id

    -- report statetable "after-close-local"


countActiveStreams :: StreamStateTable -> IO Int
countActiveStreams streamstate =
    H.foldM ( \ c _ -> return (c+1) ) 0 streamstate
