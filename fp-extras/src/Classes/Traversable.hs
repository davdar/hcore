{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Classes.Traversable where

import FPUtil.Function
import Control.Monad.Identity
import Classes.Iterable
import Classes.Indexed
import Classes.CFunctor

class (CFunctor t) => CTraversable t where
  cmapM :: (Monad m, Compat t a, Compat t b) => (a -> m b) -> t a -> m (t b)

class (IdxCFunctor t) => IdxCTraversable t where
  icmapM :: (Monad m, Compat t a, Compat t b) => (FIndex t -> a -> m b) -> t a -> m (t b)

icmapFromIcmapM :: (IdxCTraversable t, Compat t a, Compat t b) => (FIndex t -> a -> b) -> t a -> t b
icmapFromIcmapM f = runIdentity . icmapM (return .: f)

cmapMFromIcmapM :: (Monad m, IdxCTraversable t, Compat t a, Compat t b) => (a -> m b) -> t a -> m (t b)
cmapMFromIcmapM = icmapM . const
