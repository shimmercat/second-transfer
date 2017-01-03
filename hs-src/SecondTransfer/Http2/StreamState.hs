module SecondTransfer.Http2.StreamState (
                 openStream
               , closeStreamRemote
               , closeStreamLocal
               , countActiveStreams
               , getStreamState
               , getStreamLabel
               , relabelStream
               , reserveStream
               , reportActiveStreams
               , closeStreamLocalAndRemote

               , StreamState            (..)
               , StreamStateTable
                                        ) where


import qualified Data.HashTable.IO                      as H
import           Data.Word
import           Debug.Trace

import qualified Data.ByteString                        as B

type HashTable k v = H.CuckooHashTable k v

type GlobalStreamId = Int

-- Second member of the value is a possible stream label, used for
-- diagnostics
type StreamStateTable = HashTable GlobalStreamId (Word8, Maybe B.ByteString)


-- Bits:
--   0 : half-closed (local)
--   1 : half-closed (remote)
--
-- Numbers:
--   0 : open
--   1 : half-closed (local)
--   2 : half-closed (remote)
--   3 -> Delete : stream is closed or idle
--   5 : stream is reserved for push

data StreamState =
    Open_SS
  | HalfClosedLocal_SS
  | HalfClosedRemote_SS
  | Idle_SS
  | Reserved_SS
  | Closed_SS


getStreamState :: StreamStateTable -> GlobalStreamId -> GlobalStreamId -> IO StreamState
getStreamState statetable maxid queryid =
  do
    maybe_state <- H.lookup statetable queryid
    case maybe_state of
        Nothing | maxid >= queryid -> return Closed_SS
                | otherwise -> return Idle_SS
        Just (0,_) -> return Open_SS
        Just (1,_) -> return HalfClosedLocal_SS
        Just (2,_) -> return HalfClosedRemote_SS
        Just (5,_) -> return Reserved_SS
        Just _ -> do
             H.delete statetable queryid
             return $ Closed_SS


getStreamLabel :: StreamStateTable -> GlobalStreamId ->  IO (Maybe B.ByteString)
getStreamLabel statetable  queryid =
  do
    maybe_state <- H.lookup statetable queryid
    case maybe_state of
        Nothing -> return Nothing
        Just (_,maybe_label) -> return maybe_label

-- Debugging function
-- report :: StreamStateTable -> String -> IO ()
-- report statetable msg = do
--   active_streams <- countActiveStreams statetable
--   putStrLn $ "ActiveStreams: " ++ msg ++  " " ++  (show active_streams)


openStream :: StreamStateTable   -> GlobalStreamId -> Maybe B.ByteString -> IO ()
openStream statetable stream_id maybe_label = do
    maybe_state <- H.lookup statetable stream_id
    case maybe_state of
        Nothing ->
            H.insert statetable stream_id (0, maybe_label)
        Just (5,previous_label)
          | Just _ <- maybe_label ->
            H.insert statetable stream_id (0, maybe_label)
          | otherwise ->
            H.insert statetable stream_id (0, previous_label)

        Just _ -> error "InternalError:TryingToOpenStreamInIncorrectState"


reserveStream :: StreamStateTable   -> GlobalStreamId -> Maybe B.ByteString -> IO ()
reserveStream statetable stream_id maybe_label = do
    -- report statetable "before-open"
    H.insert statetable stream_id (5, maybe_label)


relabelStream :: StreamStateTable -> GlobalStreamId -> Maybe B.ByteString -> IO ()
relabelStream statetable stream_id  maybe_label = do
    maybe_state <- H.lookup statetable stream_id
    let
        new_value = case maybe_state of
            Nothing -> Nothing
            Just (n,_) -> Just (n,maybe_label)

    case new_value of
        Just x -> H.insert statetable stream_id x
        Nothing -> H.delete statetable stream_id


closeStreamRemote :: StreamStateTable -> GlobalStreamId -> IO ()
closeStreamRemote statetable stream_id = do
    maybe_state <- H.lookup statetable stream_id
    let
        new_value = case maybe_state of
            Nothing -> Nothing
            Just (0,a) -> Just (2,a)
            Just (1,_a) -> Nothing
            Just (2,a) -> Just (2,a)
            Just _ -> Nothing

    case new_value of
        Just x -> H.insert statetable stream_id x
        Nothing -> H.delete statetable stream_id


closeStreamLocal :: StreamStateTable -> GlobalStreamId -> IO ()
closeStreamLocal statetable stream_id = do
    maybe_state <- H.lookup statetable stream_id
    let
        new_value = case maybe_state of
            Nothing -> Nothing
            Just (0,a) -> Just (1,a)
            Just (1,a) -> Just (1,a)
            Just (2,_a) -> Nothing
            Just _ -> Nothing

    case new_value of
        Just x -> H.insert statetable stream_id x
        Nothing -> H.delete statetable stream_id


closeStreamLocalAndRemote :: StreamStateTable -> GlobalStreamId -> IO ()
closeStreamLocalAndRemote statetable stream_id = do
    H.delete statetable stream_id


--
countActiveStreams :: StreamStateTable -> IO Int
countActiveStreams streamstate =
    H.foldM ( \ c _ -> return (c+1) ) 0 streamstate



-- | Diagnostics function
reportActiveStreams :: StreamStateTable -> IO ()
reportActiveStreams streamstate =
    H.mapM_ ( \ (state_bits, maybe_label) ->
                  putStrLn $ (show state_bits) ++ " " ++ show maybe_label
            )
            streamstate
