module FP.Classes.Functor
  ( module FP.Classes.Functor
  , module Data.Functor
  ) where

import FP.Classes.Compat
import Prelude ()
import FP.PrePrelude
import Data.Functor
import FP.Classes.Monad

class CFunctor (t :: * -> *) where
  cmapM :: (Monad m, Compat t a, Compat t b) => (a -> m b) -> t a -> m (t b)

ceachM :: (Monad m, CFunctor t, Compat t a, Compat t b) => t a -> (a -> m b) -> m (t b)
ceachM = flip cmapM

cmap :: (CFunctor t, Compat t a, Compat t b) => (a -> b) -> t a -> t b
cmap f = runIdentity . cmapM (return . f)

ceach :: (CFunctor t, Compat t a, Compat t b) => t a -> (a -> b) -> t b
ceach = flip cmap

class (CFunctor t, Compat t ~ Universal) => TFunctor (t :: * -> *) where
  mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
  mapM = cmapM

eachM :: (Monad m, TFunctor t) => t a -> (a -> m b) -> m (t b)
eachM = ceachM

map :: (TFunctor t) => (a -> b) -> t a -> t b
map = cmap

each :: (TFunctor t) => t a -> (a -> b) -> t b
each = ceach

-- class CFunctor1 (t :: k -> * -> *) where
--   type Compat1 t :: * -> Constraint
--   cmap1M :: (Monad m, Compat1 t a, Compat1 t b) => (a -> m b) -> t i a -> m (t i b)
-- 
-- class CFunctor2 (t :: k1 -> k2 -> * -> *) where
--   type Compat2 t :: * -> Constraint
--   cmap2M :: (Monad m, Compat2 t a, Compat2 t b) => (a -> m b) -> t i j a -> m (t i j b)
