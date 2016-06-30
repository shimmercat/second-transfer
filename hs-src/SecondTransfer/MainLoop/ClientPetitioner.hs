module SecondTransfer.MainLoop.ClientPetitioner(
    ClientPetitioner(..)
                                               ) where


--import           Control.Lens
--import           Data.Conduit
-- import qualified Data.ByteString                                    as B


import SecondTransfer.MainLoop.CoherentWorker                       (HqHeaders,InputDataStream)


class ClientPetitioner a where
    request :: a -> HqHeaders -> InputDataStream -> IO (HqHeaders,InputDataStream)
