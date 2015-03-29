
{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FunctionalDependencies, GADTs #-}

module Rede.SpdyProtocol.Framing.Frame(
     cfType
    ,cfLength
    ,cfFlags
    ,getControlFrame
    ,bitsetToWord8
    ,word8ToBitset
    ,resetControlFrameSize
    ,fbs0
    ,fbs
    ,fbs1

    ,FrameControlOrData  (..)
    ,ControlFrame        (..)
    ,ControlFrameType    (..)
    ,FlagsBitSet

    ,FrameIsControlOrData(..)
    ,HasFrameFlags       (..)
    ,HasStreamId         (..)
    ) where 

import           Data.Binary         (Binary,  putWord8, get, put, Get)
-- import           Data.Binary.Builder (Builder)
import           Data.Binary.Put     (putWord16be)
import           Data.Binary.Get     (getWord16be, getWord8)
import           Data.Bits
import qualified Data.BitSet.Generic as GenericBitset
-- import qualified Data.ByteString     as B
-- import           Data.Monoid
import           Data.Word
import           Rede.Utils(getWord24be, putWord24be)


type FlagsBitSet = GenericBitset.BitSet Word8

fbs0 :: Enum a => FlagsBitSet a 
fbs0 = GenericBitset.empty

fbs :: Enum a => [a] -> FlagsBitSet a 
fbs l = GenericBitset.fromList l

fbs1 :: Enum a => a -> FlagsBitSet a 
fbs1 flag = GenericBitset.singleton flag


class HasFrameFlags frame flag | frame -> flag where 
    applyFrameFlag :: frame -> flag -> Bool -> frame
    getFrameFlag :: frame -> flag -> Bool

    -- Auto
    setFrameFlag :: frame -> flag -> frame 
    setFrameFlag frame flag = applyFrameFlag frame flag True  


class HasStreamId a where 
    streamIdFromFrame :: a -> Int


data FrameControlOrData = 
    FrameIsControl
    |FrameIsData
    deriving (Show,Enum)


class FrameIsControlOrData a where 
    isControlOrData:: a -> FrameControlOrData


data ControlFrameType = 
    Zero_CFT -- Not a valid control frame
    |SynStream_CFT 
    |SynReply_CFT
    |RstStream_CFT
    |Settings_CFT
    |Deprecated1_CFT
    |Ping_CFT
    |GoAway_CFT
    |Headers_CFT
    |WindowUpdate_CFT
    deriving (Show, Enum)


data ControlFrameFlags = 
    Null_CFF
    |Fin_CFF
    deriving (Show, Enum)


data ControlFrame valid_flags where 
     ControlFrame ::  
        Enum valid_flags => 
        ControlFrameType 
        -> FlagsBitSet valid_flags 
        -> Int -- Length
        -> ControlFrame valid_flags 


deriving instance Show valid_flags => Show (ControlFrame valid_flags)


resetControlFrameSize :: ControlFrame vf -> Int -> ControlFrame vf 
resetControlFrameSize (ControlFrame a b _) new_size =
    (ControlFrame a  b new_size) 


cfType :: ControlFrame vf -> ControlFrameType 
cfType (ControlFrame cftype _ _) = cftype 

cfFlags :: ControlFrame vf -> FlagsBitSet vf 
cfFlags (ControlFrame _ flags _) = flags

cfLength :: ControlFrame vf -> Int 
cfLength (ControlFrame _ _ l) = l


instance Enum valid_flags => Binary (ControlFrame valid_flags) where 
    put cf = 
        do 
            let 
                frame_length = cfLength cf 
                flags_word = bitsetToWord8 $ cfFlags cf    
            putWord16be $ controlSet 3 -- Set the version in the first two bytes
            putWord16be $ fromIntegral $ fromEnum $ cfType cf
            putWord8 $ flags_word
            putWord24be frame_length

    get  = do 
        getWord16be 
        type_int <- getWord16be
        flags_word <- getWord8
        frame_length <- getWord24be
        return $ let
                    actual_flags   = word8ToBitset flags_word
                    in ControlFrame 
                        (toEnum $ fromIntegral type_int) 
                        actual_flags
                        frame_length


getControlFrame :: Get (ControlFrame ControlFrameFlags)
getControlFrame = get

 
controlSet :: Word16 -> Word16
controlSet = (32768 .|. ) 


bitsetToWord8 :: Enum valid_flags => 
    FlagsBitSet valid_flags -> Word8
bitsetToWord8 flags_bitset = 
    foldl (.|.) 0 shifted_bits
  where
    bitset_as_list = GenericBitset.toList $ flags_bitset
    shifted_bits = map flag_to_word8 bitset_as_list
    flag_to_word8 = (fromIntegral . (shiftL (1::Word8)) . fromEnum) 


word8ToBitset :: Enum valid_flags =>
    Word8 -> FlagsBitSet valid_flags
word8ToBitset word8 = 
    GenericBitset.fromList flags_list
  where
    -- Lazy evaluation avoids errors here for short enumerations
    numbers = [0..7]
    possible_flags = map toEnum numbers
    sieves  = map (\ x -> 1 `shiftL` x) numbers
    filter_present_pairs = filter (\ (_, sieve) -> (sieve .&. word8) /= 0 )
    flags_list = map fst $ filter_present_pairs  $ zip possible_flags sieves