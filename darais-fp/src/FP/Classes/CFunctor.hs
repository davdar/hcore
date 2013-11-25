module FP.Classes.CFunctor where

import Prelude ()

class CFunctor t where
  type Compat t :: * -> Constraint
  cmapM :: (Monad m, Compat t a, Compat t b) => (a -> m b) -> t a -> m (t b)

cmap :: (Compat t a, Compat t b) => (a -> b) -> t a -> t b
cmap f = runIdentity . cmapM (Identity . f)
