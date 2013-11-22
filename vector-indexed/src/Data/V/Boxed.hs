{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.V.Boxed
  ( V
  , empty, singleton, fromL, build, buildM
  , fill, fillM, iterateN, iterateNM
  ) where

import Data.L (L)
import Prelude ()
import FP
import Data.Int.Indexed
import qualified Data.V.Generic as V
import Data.V.Generic (IVector, UnIndexedV)
import Data.Vector (Vector)

newtype V (i::Nat) a = V { unV :: Vector a }
  deriving (Eq, Ord)

instance IVector V a where
  type UnIndexedV V = Vector
  stripIV = unV
  unsafeIV = V
instance Iterable (V i a) where
  type Elem (V i a) = a
  iterOnL = iterOnLFromIiter
  iterOnR = iterOnRFromIiter
instance IdxIterable (V i a) where
  type Index (V i a) = BInt i
  iiterOnL = V._iiterOnL
  iiterOnR = V._iiterOnR
  (!) = V._index
instance CFunctor (V i) where
  type Compat (V i) = Universal
  cmap = cmapFromIcmap
instance IdxCFunctor (V i) where
  type FIndex (V i) = BInt i
  icmap = icmapFromIcmapM
instance CTraversable (V i) where
  cmapM = cmapMFromIcmapM
instance IdxCTraversable (V i) where
  icmapM = V._icmapM
instance (Pretty a) => Pretty (V i a) where
  pretty = V._pretty prettyDropIndent
instance (Pretty a) => PrettyExI V a where
  prettyExI = pretty
instance (Show a) => Show (V i a) where
  show = showPretty . V._pretty prettyFromShow

empty :: V 0 a
empty = V.empty

singleton :: a -> V 1 a
singleton = V.singleton

fromL :: L i a -> V i a
fromL = V.fromL

build :: SInt i -> (BInt i -> a) -> V i a
build = V.build

buildM :: (Monad m) => SInt i -> (BInt i -> m a) -> m (V i a)
buildM = V.buildM

fill :: SInt i -> a -> V i a
fill = V.fill

fillM :: (Monad m) => SInt i -> m a -> m (V i a)
fillM = V.fillM

iterateN :: SInt i -> a -> (a -> a) -> V i a
iterateN = V.iterateN

iterateNM :: (Monad m) => SInt i -> a -> (a -> m a) -> m (V i a)
iterateNM = V.iterateNM
