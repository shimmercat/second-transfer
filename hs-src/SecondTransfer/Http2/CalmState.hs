{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module SecondTransfer.Http2.CalmState (
                CalmState
              , newCalmState
              , getCurrentCalm
              , advanceCalm

              , CalmEnhacementMap
    ) where

--import          Control.Lens
--import          Data.Word                   (Word)

--- How to enhance the calm as word boundaries are crossed.
type CalmEnhacementMap = [(Word, Word)]


-- Struct is private,
data CalmState = CalmState {
    _restOfMap     :: !CalmEnhacementMap
  , _currentOffset :: !Int
  , _currentCalm   :: !Int
    }
    deriving (Show, Eq)

newCalmState :: Int -> CalmEnhacementMap -> CalmState
newCalmState start_calm cemap = CalmState cemap 0 start_calm


getCurrentCalm :: CalmState -> Int
getCurrentCalm CalmState{_currentCalm} = _currentCalm


advanceCalm :: CalmState -> Int -> CalmState
advanceCalm cs@CalmState{ _restOfMap = [] } _advance_bytes = cs
advanceCalm _cs@CalmState{ _restOfMap = rom@((offset, bump):r),
                          _currentOffset,
                          _currentCalm } advance_bytes =
  let
    new_offset = _currentOffset + advance_bytes
    crosses_boundary = fromIntegral new_offset > offset
  in
    CalmState {
        _restOfMap = if crosses_boundary then r else rom ,
        _currentOffset = new_offset,
        _currentCalm = if crosses_boundary then _currentCalm + fromIntegral bump else _currentCalm
    }
