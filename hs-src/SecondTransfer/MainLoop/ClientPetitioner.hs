module SecondTransfer.MainLoop.ClientPetitioner(
    ClientPetitioner(..)
                                               ) where


--import           Control.Lens
--import           Data.Conduit
-- import qualified Data.ByteString                                    as B


import SecondTransfer.MainLoop.CoherentWorker                       (Headers,InputDataStream)


class ClientPetitioner a where
    request :: a -> Headers -> InputDataStream -> IO (Headers,InputDataStream)
