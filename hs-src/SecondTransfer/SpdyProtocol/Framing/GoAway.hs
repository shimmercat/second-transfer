{-# LANGUAGE StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module SecondTransfer.SpdyProtocol.Framing.GoAway (
	GoAwayValidFlags(..)
	,GoAwayFrame(..)
    ,GoAwayReason(..)
	) where 



import           Data.Word
import           Data.Binary                    (Binary, Get, get, put)
import           SecondTransfer.SpdyProtocol.Framing.Frame
import           Data.Binary.Get                (getWord32be)
import           Data.Binary.Put                (putWord32be)
import           Data.Default 


data GoAwayValidFlags = None_GAVF    
    deriving (Show, Enum)


data GoAwayReason = OK_GAR
                   |ProtocolError_GAR
                   |InternalError_GAR
    deriving (Show, Enum)


data GoAwayFrame = GoAwayFrame {
    prologue:: ControlFrame GoAwayValidFlags
    , lastGoodStream:: Int
    , statusCode:: GoAwayReason
  } deriving Show


instance Default (ControlFrame GoAwayValidFlags) where 
    def =  ControlFrame GoAway_CFT (fbs1 None_GAVF) 8


instance Binary GoAwayFrame where

    put (GoAwayFrame pr lgs sc) = do
        put newprologue 
        putWord32be (fromIntegral lgs::Word32 )
        putWord32be $ (fromIntegral . fromEnum ) sc
      where 
        newprologue = resetControlFrameSize pr 8

    get = do 
        pr  <- get :: Get (ControlFrame GoAwayValidFlags)
        lgi <- getWord32be 
        sc  <- getWord32be  
        return $ GoAwayFrame {
            prologue = pr
            ,lastGoodStream = fromIntegral lgi
            ,statusCode = (toEnum . fromIntegral) sc 
        }

