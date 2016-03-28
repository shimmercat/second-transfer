{-# LANGUAGE TypeFamilies, GADTs, TemplateHaskell #-}
{-
Module: SecondTransfer.Sessions.RequesterPool

Tools to manage a pool of requesters, and the different ways we
can go about that .

This is basically a generic machinery that can be used to manage
HTTP/1.1 requests to proxied applications, HTTP/2 requests, FastCGI,
etc.

-}
module SecondTransfer.Sessions.RequesterPool (
                 RequesterPool                                     (..)

               , PoolOfOne
               , makePoolOfOne

               , InfinitePool
               , makeInfinitePool
      ) where


import           Control.Lens
import           Control.Concurrent

import qualified Data.Vector                                       as Vec

import           System.Random



-- SEC-1...................................................#########################################
-- The BusyState class -----------------------------------------------------------------------------

-- | How to know if an element of the pool is busy or not?
-- Be carefull with using this predicate for elements of a concurrent
-- container without holding some kind of lock.
class BusyState a where
    isBusy :: a -> Bool

-- | A type, together with (type-free) operations to mark it busy
--  and to mark it free, and to obtain some inner resource.
type BusySwitches m busy free  = ( (free -> m busy ), ( busy -> m free) )


-- SEC-2...................................................#########################################
-- The RequesterPool class and its instances--------------------------------------------------------


-- | Basic interface to request something
class RequesterPool rp where

    -- | A type representing the resource we can obtain.
    type Requester rp :: *

    -- | Gets and declares busy a requester, together with
    --   a method to dispose of it.
    --    Use that method to register an eager finalizer in
    --   a resourceT monad.
    --  This method may block
    getRequesterFromPool :: rp -> IO (Requester rp, IO ())

    -- | Just as before, but without blocking. If there is no
    --   requester available, just return Nothing
    getRequesterIfAvailable :: rp -> IO (Maybe (Requester rp, IO ()))


-- SEC-2.1 .................................................#########################################
-- PoolOfOne ----------------------------------------------------------------------------------------


-- | Basic instance where we have just one something that can be busy or not
--   Our 'busy' type is the one associated with the requester
data PoolOfOne busy free = PoolOfOne {
    _switches_PoO    :: BusySwitches IO busy free
  --, _busy_PoO        :: MVar busy
  , _free_PoO        :: MVar free
    }

makeLenses ''PoolOfOne

makePoolOfOne :: BusySwitches IO busy free -> free -> IO (PoolOfOne busy free)
makePoolOfOne switches fr =
  do
    fr_mvar <- newMVar fr
    return PoolOfOne {
        _switches_PoO = switches
      , _free_PoO = fr_mvar
        }


instance RequesterPool (PoolOfOne busy free) where
    type Requester (PoolOfOne busy free) = busy

    getRequesterFromPool pool_of_one = do
        free_thing <- takeMVar (pool_of_one ^. free_PoO)
        busy_thing <- (pool_of_one ^. switches_PoO . _1) free_thing

        let
            make_it_free = do
                free_thing <- (pool_of_one ^. switches_PoO . _2) busy_thing
                putMVar (pool_of_one ^. free_PoO) free_thing

        return (busy_thing, make_it_free)

    getRequesterIfAvailable pool_of_one = do
        maybe_free_thing <- tryTakeMVar (pool_of_one ^. free_PoO)

        case maybe_free_thing of
            Nothing -> return Nothing
            Just free_thing ->  do
                busy_thing <- (pool_of_one ^. switches_PoO . _1) free_thing

                let
                    make_it_free = do
                        free_thing <- (pool_of_one ^. switches_PoO . _2) busy_thing
                        putMVar (pool_of_one ^. free_PoO) free_thing
                return $ Just (busy_thing, make_it_free)

-- SEC-2.1 .................................................#########################################
-- InfinitePool--------------------------------------------------------------------------------------

-- | Basic instance where resources are created and fred on demand,
--  up to an infinite number
data InfinitePool r = InfinitePool {
    _create_InP        :: IO r
  , _free_InP          :: r -> IO ()
    }

makeLenses ''InfinitePool

instance RequesterPool (InfinitePool r) where
    type Requester (InfinitePool r) = r

    getRequesterFromPool inp =
      do
        rq <- inp ^. create_InP
        let
            free_it = (inp ^. free_InP) rq

        return (rq, free_it)

    getRequesterIfAvailable inp =
       Just `fmap` getRequesterFromPool inp

makeInfinitePool :: (IO r) -> (r -> IO() ) -> InfinitePool r
makeInfinitePool create destroy = InfinitePool create destroy

-- SEC-3...................................................#########################################
-- The SelectionPolicy -----------------------------------------------------------------------------

-- | When I need to select one... I do recommend executing this
--   in some kind of lock.
class SelectionPolicy a where
    pickOne :: BusyState b =>  a -> Vec.Vector b ->  (a, Maybe (Int, b) )


-- | When I need to select one and I'm fine with it being a random element
newtype RandomSelection = RandomSelection StdGen


-- | Create and seed new  RandomSelection
newRandomSelectionPolicy :: IO RandomSelection
newRandomSelectionPolicy =
    getStdRandom $ \ stdgen ->
         let
             (seed, newglobal) = random stdgen ::  (Int, StdGen)
             gen = mkStdGen seed
         in (RandomSelection gen, newglobal)


-- | The instance....
instance SelectionPolicy RandomSelection where
    pickOne r@(RandomSelection stdgen) vec_of_goodies =
      let
        free_elems = Vec.filter (not <$> isBusy) vec_of_goodies
        n = Vec.length free_elems
        (the_chosen_idx, new_r) = randomR (0, n-1) stdgen
        the_chosen = free_elems Vec.! the_chosen_idx
      in if n == 0
        then
          (r, Nothing)
        else
          (RandomSelection new_r, Just (the_chosen_idx, the_chosen))


-- Left to do: we need the "go-for-first policy", and the composition containers to
-- be able to handle any load at all.
