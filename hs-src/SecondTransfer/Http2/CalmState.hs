{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module SecondTransfer.Http2.CalmState (
                CalmState
              , newCalmState
              , getCurrentCalm
              , getCurrentPause
              , advanceCalm
              , bytesTillNextCalmSwitch

              , CalmEnhacementMap
    ) where

--import          Control.Lens
--import          Data.Word                   (Word)

--- How to enhance the calm as word boundaries are crossed.
type CalmEnhacementMap = [(Word, Word, Int)]


-- Struct is private,
data CalmState = CalmState {
    _restOfMap     :: !CalmEnhacementMap
  , _currentOffset :: !Int
  , _currentCalm   :: !Int
  , _makePause     :: !Int
    }
    deriving (Show, Eq)


newCalmState :: Int -> CalmEnhacementMap -> CalmState
newCalmState start_calm cemap = CalmState {
   _restOfMap = cemap,
   _currentOffset = 0,
   _currentCalm = start_calm,
   _makePause = 0
   }


getCurrentCalm :: CalmState -> Int
getCurrentCalm CalmState{_currentCalm} = _currentCalm


bytesTillNextCalmSwitch :: CalmState ->  Int
bytesTillNextCalmSwitch  CalmState{
  _restOfMap = []
  } = 4000000000             -- Not good, will cause problems
bytesTillNextCalmSwitch  CalmState{
  _restOfMap = _rom@((offset, _bump,_prepause):_r),
  _currentOffset,
  _currentCalm }  =  fromIntegral offset -  _currentOffset


getCurrentPause :: CalmState -> Int
getCurrentPause CalmState{_makePause} = _makePause


advanceCalm :: CalmState -> Int -> CalmState
advanceCalm cs@CalmState{ _restOfMap = [] } _advance_bytes = cs
advanceCalm _cs@CalmState{ _restOfMap = rom@((offset, bump, prepause):r),
                          _currentOffset,
                          _makePause,
                          _currentCalm } advance_bytes =
  let
    new_offset = _currentOffset + advance_bytes
    crosses_boundary = fromIntegral new_offset >= offset
  in
    CalmState {
        _restOfMap = if crosses_boundary then r else rom ,
        _currentOffset = new_offset,
        _makePause = if crosses_boundary then prepause else _makePause,
        _currentCalm = if crosses_boundary then _currentCalm + fromIntegral bump else _currentCalm
    }
