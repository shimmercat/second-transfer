{-# LANGUAGE ExistentialQuantification, TemplateHaskell, DeriveDataTypeable #-}
module SecondTransfer.IOCallbacks.SaveFragment (
                 SaveReadFragment
               , newSaveReadFragment
       ) where

import           Control.Lens
import           Control.Concurrent

--import           Data.IORef
--import           Data.Typeable

import qualified Data.ByteString                              as B
import qualified Data.ByteString.Lazy                         as LB
--import qualified Data.ByteString.Builder                      as Bu

import           SecondTransfer.IOCallbacks.Types


data SaveReadFragment = SaveReadFragment {
    _inner_SRF          :: IOCallbacks
  , _fragment_SRF       :: MVar B.ByteString
    }

makeLenses ''SaveReadFragment


pushAction :: SaveReadFragment -> LB.ByteString -> IO ()
pushAction srf what = (srf ^.  inner_SRF . pushAction_IOC ) what


bestEffortPullAction :: SaveReadFragment -> Bool -> IO B.ByteString
bestEffortPullAction srf can_block  = do
    maybe_fragment <- tryTakeMVar (srf ^. fragment_SRF )
    case maybe_fragment of
        Nothing -> (srf ^. inner_SRF . bestEffortPullAction_IOC ) can_block
        Just fragment -> return fragment


closeAction :: SaveReadFragment -> IO ()
closeAction srf = do
    tryTakeMVar (srf ^. fragment_SRF)
    srf ^. inner_SRF . closeAction_IOC


newSaveReadFragment :: IOCallbacks ->  B.ByteString -> IO SaveReadFragment
newSaveReadFragment io_callbacks fragment = do
    fragment_mvar <- newMVar fragment
    return SaveReadFragment {
        _inner_SRF = io_callbacks
      , _fragment_SRF = fragment_mvar
        }


instance IOChannels SaveReadFragment where

    handshake srf = do
        pull_action_wrapping <- newPullActionWrapping $ bestEffortPullAction srf

        return $ IOCallbacks {
            _pushAction_IOC = pushAction srf
          , _pullAction_IOC = pullFromWrapping pull_action_wrapping
          , _bestEffortPullAction_IOC = bestEffortPullAction srf
          , _closeAction_IOC = closeAction srf
            }
