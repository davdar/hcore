{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Classes.Indexed where

import Classes.CFunctor
import FPUtil.Function

class Indexed (t::k -> *) where
  type UnIndexed t :: *
  stripI :: t i -> UnIndexed t
  unsafeI :: UnIndexed t -> t i

class (CFunctor t) => IdxCFunctor t where
  type FIndex t :: *
  icmap :: (Compat t a, Compat t b) => (FIndex t -> a -> b) -> t a -> t b

iceach :: (IdxCFunctor t, Compat t a, Compat t b) => t a -> (FIndex t -> a -> b) -> t b
iceach = flip icmap

cmapFromIcmap :: (IdxCFunctor t, Compat t a, Compat t b) => (a -> b) -> t a -> t b
cmapFromIcmap = icmap . const
