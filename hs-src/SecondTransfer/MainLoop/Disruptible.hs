{-# LANGUAGE ExistentialQuantification #-}
module SecondTransfer.MainLoop.Disruptible (
                 Disruptible        (..)
               , MonoDisruptible    (..)
       ) where


-- | This is an entity that can be disrupted and closed. Good to
--   force shutdown in some places.
class Disruptible d where
    disrupt :: d -> IO ()


-- | Monomorphic encapsulation of a Disruptible
data MonoDisruptible = forall d . Disruptible d => MonoDisruptible d
