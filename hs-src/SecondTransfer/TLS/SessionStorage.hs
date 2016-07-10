{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module SecondTransfer.TLS.SessionStorage (
       TLSSessionStorage                             (..)

     , StorageCRecord                                (..)
     , pSave_Pre
     , pRemoveEntry_Pre
     , pSessionLifetime_Pre
     , pEncryptionKey_Pre
     , pLoad_Pre

     , instanceToStorageCRecord
     , freeStorageCRecord

     , Save_Pre
     , RemoveEntry_Pre
     , Load_Pre
     , SessionLifetime_Pre
     , EncryptionKey_Pre
    ) where

-- Deals with session resumption storage

import           Control.Lens

import qualified Data.ByteString                     as B
import qualified Data.ByteString.Unsafe              as UB
import           Foreign
import           Foreign.C
--import           Foreign.Storable
--import           Foreign.Marshal
--import           Foreign.Ptr


-- |  Class for storing TLS session data. Be carefull
--    with this
class TLSSessionStorage a where
    -- | save storage key encrpyted_session
    save :: a -> B.ByteString -> B.ByteString -> IO ()
    -- | removeEntry storage key
    removeEntry :: a -> B.ByteString -> IO ()
    -- | loadEntry storage key -> encrpyed_session
    load :: a -> B.ByteString -> IO (Maybe B.ByteString)
    -- | Returns lifetime in seconds
    sessionLifetime :: a -> Int
    -- | Returns an encryption key, any length
    encryptionKey :: a -> B.ByteString


type Save_Pre = Ptr CChar -> CInt -> Ptr CChar -> CInt -> IO ()
type RemoveEntry_Pre = Ptr CChar -> CInt -> IO ()
type Load_Pre  = Ptr CChar -> CInt -> Ptr (Ptr CChar) -> Ptr CInt -> IO ()
type SessionLifetime_Pre = IO CInt
type EncryptionKey_Pre = Ptr (Ptr CChar) -> Ptr CInt -> IO ()


foreign import ccall "wrapper" createSave_Pre             :: Save_Pre              -> IO (FunPtr Save_Pre )
foreign import ccall "wrapper" createRemoveEntry_Pre      :: RemoveEntry_Pre       -> IO (FunPtr RemoveEntry_Pre)
foreign import ccall "wrapper" createLoad_Pre             :: Load_Pre              -> IO (FunPtr Load_Pre)
foreign import ccall "wrapper" createSessionLifetime_Pre  :: SessionLifetime_Pre   -> IO (FunPtr SessionLifetime_Pre)
foreign import ccall "wrapper" createEncryptionKey_Pre    :: EncryptionKey_Pre     -> IO (FunPtr EncryptionKey_Pre)



-- | For instances of the above class, a way to export it
--   to C code ....
data StorageCRecord = StorageCRecord {
    _pSave_Pre             ::                              FunPtr Save_Pre,
    _pRemoveEntry_Pre      ::                        FunPtr RemoveEntry_Pre,
    _pLoad_Pre             ::                               FunPtr Load_Pre,
    _pSessionLifetime_Pre  ::                    FunPtr SessionLifetime_Pre,
    _pEncryptionKey_Pre    ::                      FunPtr EncryptionKey_Pre
  }

makeLenses ''StorageCRecord

createSaveSm :: TLSSessionStorage a => a -> IO (FunPtr Save_Pre)
createSaveSm a = createSave_Pre $ \ pch_keyname keyname_length pch_tostore tostore_length -> do
    key <- B.packCStringLen (pch_keyname, fromIntegral keyname_length)
    tostore <- B.packCStringLen (pch_tostore, fromIntegral tostore_length)
    save a key tostore


createRemoveEntrySm :: TLSSessionStorage a => a ->                     IO ( FunPtr RemoveEntry_Pre)
createRemoveEntrySm a = createRemoveEntry_Pre $ \ pch_keyname keyname_length  -> do
    key <- B.packCStringLen (pch_keyname, fromIntegral keyname_length)
    removeEntry a key

createLoadSm :: TLSSessionStorage a => a ->                            IO ( FunPtr Load_Pre)
createLoadSm a = createLoad_Pre $ \ pch_keyname keyname_length ppch_value_ptr pvalue_length -> do
    key <- B.packCStringLen (pch_keyname, fromIntegral keyname_length)
    loaded_value <- load a key
    case loaded_value of
        Nothing -> do
            poke ppch_value_ptr nullPtr
            poke pvalue_length 0
        Just value -> do
            let
                required_bytes = B.length value
            UB.unsafeUseAsCString value $ \ pch_value -> do
                pnew_ch <- mallocBytes required_bytes
                copyArray pnew_ch pch_value required_bytes
                poke ppch_value_ptr pnew_ch
                poke pvalue_length $ fromIntegral required_bytes


createSessionLifetimeSm :: TLSSessionStorage a => a ->                 IO ( FunPtr SessionLifetime_Pre)
createSessionLifetimeSm a = createSessionLifetime_Pre . return . fromIntegral . sessionLifetime $ a


createEncryptionKeySm :: TLSSessionStorage a => a ->                   IO ( FunPtr EncryptionKey_Pre)
createEncryptionKeySm a = createEncryptionKey_Pre $ \ ppch_key_ptr p_keylength -> do
    let
        enc_key = encryptionKey a
        enc_key_length = B.length enc_key
    UB.unsafeUseAsCString enc_key  $ \ p_enc_key -> do
        pnew_ch <- mallocBytes enc_key_length
        copyArray pnew_ch p_enc_key enc_key_length
        poke ppch_key_ptr pnew_ch
        poke p_keylength $ fromIntegral enc_key_length



-- | Don't forget to call the free function below
instanceToStorageCRecord :: TLSSessionStorage a => a -> IO StorageCRecord
instanceToStorageCRecord a = do
    pSave_p             <-                               createSaveSm a
    pRemoveEntry_p      <-                        createRemoveEntrySm a
    pLoad_p             <-                               createLoadSm a
    pSessionLifetime_p  <-                    createSessionLifetimeSm a
    pEncryptionKey_p    <-                      createEncryptionKeySm a

    return StorageCRecord {
        _pSave_Pre = pSave_p,
        _pRemoveEntry_Pre = pRemoveEntry_p,
        _pSessionLifetime_Pre = pSessionLifetime_p,
        _pEncryptionKey_Pre = pEncryptionKey_p,
        _pLoad_Pre  = pLoad_p
        }


freeStorageCRecord :: StorageCRecord -> IO ()
freeStorageCRecord (StorageCRecord f1 f2 f3 f4 f5) = do
    freeHaskellFunPtr f1
    freeHaskellFunPtr f2
    freeHaskellFunPtr f3
    freeHaskellFunPtr f4
    freeHaskellFunPtr f5
