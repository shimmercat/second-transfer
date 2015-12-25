{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module SecondTransfer.Http2.OutputTray (
                 TrayEntry                                  (..)
               , systemPriority_TyE
               , streamPriority_TyE
               , streamOrdinal_TyE
               , payload_TyE
               , streamId_TyE

               , OutputTray                                 (..)
               , filling_OuT
               , maxLength_OuT
               , entries_OuT

               , newOutputTray
               , splitOverSize
               , addEntry
     ) where

import           Control.Lens
--import           Control.Concurrent

-- import qualified Data.Vector.Mutable                         as MVec
import qualified Data.Vector                                 as DVec
import qualified Data.Vector.Algorithms.Merge                as Dam


--import qualified Data.ByteString                             as B
import qualified Data.ByteString.Lazy                        as LB


-- | The output tray.... all data is sorted according to the priority
--   assigned here ....
--
--
--   Priorities:  0 for data frames
--               -1 for header and other HTTP/2 low level
--               -2 for go-away frame. The deliverer is expected to close
--                  the connection inmmediately after.
data TrayEntry = TrayEntry {
    _systemPriority_TyE           :: !Int
  , _streamPriority_TyE           :: !Int
  , _streamOrdinal_TyE            :: !Int

  -- Informative ....
  , _payload_TyE                  :: !LB.ByteString
  , _streamId_TyE                 :: !Int
  -- Maybe: delivery callback
    }
    deriving (Show)

makeLenses ''TrayEntry

type EntryListBuilder = [TrayEntry] -> [TrayEntry]

data OutputTray = OutputTray {
    _filling_OuT         :: Int
  , _maxLength_OuT       :: Int
  , _entries_OuT         :: !EntryListBuilder
    }

makeLenses ''OutputTray

trayCompareKey :: TrayEntry -> (Int, Int)
trayCompareKey te =
  (te ^. systemPriority_TyE, te ^. streamPriority_TyE )


trayEntryCompare :: TrayEntry -> TrayEntry -> Ordering
trayEntryCompare te0 te1 = compare (trayCompareKey te0) (trayCompareKey te1)


newOutputTray ::  Int -> OutputTray
newOutputTray max_length =
    OutputTray {
        _filling_OuT = 0
      , _maxLength_OuT = max_length
      , _entries_OuT = id
    }


-- You are in carge of protecting the access to the tray!
addEntry :: OutputTray -> TrayEntry -> OutputTray
addEntry tray entry =
  let
    ot1 = over entries_OuT ( . (entry : ) ) tray
    ot2 = over filling_OuT ( + 1 ) ot1
  in ot2


splitOverSize :: Int -> OutputTray -> (OutputTray, [TrayEntry])
splitOverSize sz ot0 =
  let
    entries_list = (ot0 ^. entries_OuT) []
    -- First let's get a vector from here
    entries_vector = DVec.fromList entries_list
    had_entries_count = DVec.length entries_vector
    -- Then let's sort it, stably so that ordinals are automatically
    -- kept (otherwise the data will break), but system and high priority
    -- ones go before everybody else
    sorted_entries :: DVec.Vector TrayEntry
    sorted_entries =
        DVec.modify
        (\ mvec -> Dam.sortBy trayEntryCompare mvec)
        entries_vector

    -- And let' returns the ones which are interesting....
    go :: Int -> Int -> Int
    go taken_size idx
      | taken_size < sz && idx < DVec.length sorted_entries
          = go (taken_size + (sorted_entries DVec.! idx) ^. payload_TyE . to (fromIntegral . LB.length) ) (idx+1)
      | taken_size >= sz
          = idx
      | otherwise
          = idx

    take_this_many = go 0 0
    entries_to_take = DVec.toList . DVec.take take_this_many $ sorted_entries
    entries_to_leave = DVec.drop take_this_many sorted_entries

    entries_to_leave_list_builder :: [TrayEntry] -> [TrayEntry]
    entries_to_leave_list_builder incoming = DVec.foldr' (:) incoming entries_to_leave
    new_tray =
      (set filling_OuT (had_entries_count - take_this_many) ) .
      (set entries_OuT entries_to_leave_list_builder)
      $ ot0
  in (new_tray, entries_to_take)
