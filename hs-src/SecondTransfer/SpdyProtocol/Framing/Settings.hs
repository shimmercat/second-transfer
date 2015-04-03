{-# LANGUAGE StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module SecondTransfer.SpdyProtocol.Framing.Settings (
     getSettingsFrame
     ,getDefaultWindowSize

    ,SettingsFrame(..)
    ,SettingsValidFlags(..)
    ,SettingsSettingsId(..)
    ,SettingItem
    ,PersistSettings(..)
    ) where 


import           Data.Word                       (Word32)
import           Data.List                       (find)
import           SecondTransfer.SpdyProtocol.Framing.Frame
import           Control.Monad
import           Data.Binary                     (Binary, Get, get, getWord8,
                                                  put, putWord8)
import           SecondTransfer.Utils
import           Data.Binary.Get                 (getWord32be)
import           Data.Binary.Put                 (putWord32be)
import           Data.BitSet.Generic             (delete, insert, member)
import           Data.Default



data SettingsValidFlags =
     None_SVF 
    |Clear_SVF
    deriving (Show, Enum)


data SettingsFrame =
    SettingsFrame {
        prologue:: ControlFrame SettingsValidFlags
        ,persistSettings:: [SettingItem]
    }
    deriving Show


instance HasFrameFlags SettingsFrame SettingsValidFlags where 

    applyFrameFlag frame flag set_value = frame {
        prologue = newpr }
      where 
        (ControlFrame cft flags len) = prologue frame 
        newpr = if set_value 
          then ControlFrame cft (insert flag flags) len
          else ControlFrame cft (delete flag flags) len 

    getFrameFlag frame flag = let 
        (ControlFrame _ flags _) = prologue frame 
      in member flag flags



instance Default (ControlFrame SettingsValidFlags) where 
  def = ControlFrame Settings_CFT (fbs1 None_SVF) (-1)


data SettingsSettingsId =
    Null_S 
    |UploadBandwith_S
    |DownloadBandwith_S
    |RoundTripTime_S
    |MaxConcurrentStreams_S
    |CurrentCwnd_S
    |DownloadRetransRate_S
    |InitialWindowSize_S
    |ClientCertificateVectorSize_S
    deriving (Enum,Show, Eq)


type SettingItem = (SettingsSettingsId, Word32, PersistSettings) 


getDefaultWindowSize :: SettingsFrame -> Maybe Int 
getDefaultWindowSize (SettingsFrame _ persist) = 
    case find (\ (setting_id, _ , _) -> setting_id == InitialWindowSize_S ) persist of 
        Just (_, v, _)    ->   Just $ fromIntegral v 
        Nothing           ->   Nothing



data PersistSettings = 
    None_PS
    |PersistValue_PS
    |PersistedValue_PS
    deriving (Enum,Show)


instance Binary SettingsFrame where 
    put SettingsFrame{prologue=pr, persistSettings=psh} = 
      do 
        let 
          entry_count = length psh 
          -- Create a different prologue with a correct size, 
          -- since we are not sure this one is...
          frame_size = 8*entry_count + 4
          new_pr = resetControlFrameSize pr frame_size
        put new_pr
        putWord32be $ fromIntegral entry_count
        forM_  psh $ \ (key,value, persist) -> 
          do 
            let 
              persist_setting = fromEnum persist
            putWord8 $ fromIntegral persist_setting
            putWord24be $ fromEnum key 
            putWord32be $ fromIntegral value


    get = 
      do
        pr <- get 
        entry_count <- getWord32be
        settings_themselves <- replicateM (fromIntegral entry_count) $ do 
          persist_setting_w8 <- getWord8
          key_24 <- getWord24be 
          value  <- getWord32be
          return (
            (toEnum key_24),
            value,
            (toEnum $ fromIntegral persist_setting_w8) 
            )
        return $ SettingsFrame pr settings_themselves


getSettingsFrame :: Get SettingsFrame
getSettingsFrame = get



-- settingsFrame :: Int -> Int -> SettingsFrame
-- settingsFrame  stream_id status_code = 
--   SettingsFrame 
--     (ControlFrame Settings_CFT empty 8)
--     stream_id status_code