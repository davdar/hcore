{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.M.Prim where

import Unsafe.Coerce
import Prelude ()
import FP
import Data.V.Prim (V, Prim)
import qualified Data.V.Generic as V
import Data.M.Generic
import Data.Int.Indexed
import Data.Packed.Matrix (Matrix, Element)
import Foreign.Storable
import qualified Data.Packed.Matrix as HM
import qualified Numeric.LinearAlgebra.Algorithms as HM

data M i j a = M 
  { mRows :: {-# UNPACK #-} !(SInt i)
  , mCols :: {-# UNPACK #-} !(SInt j)
  , mData :: {-# UNPACK #-} !(V (i*j) a)
  }

instance (Prim a) => IMatrix M a where
  type FlatM M = V
  rollM = M
  unrollM = mData
  rowsM = mRows
  colsM = mCols
instance (Prim a) => Iterable (M i j a) where
  type Elem (M i j a) = a
  iterOnL = iterOnLFromIiter
  iterOnR = iterOnRFromIiter
instance (Prim a) => IdxIterable (M i j a) where
  type Index (M i j a) = (BInt i, BInt j)
  iiterOnL = _iiterOnL
  iiterOnR = _iiterOnR
  (!) = _index
instance CFunctor (M i j) where
  type Compat (M i j) = Prim
  cmap = cmapFromIcmap
instance IdxCFunctor (M i j) where
  type FIndex (M i j) = (BInt i, BInt j)
  icmap = icmapFromIcmapM
instance CTraversable (M i j) where
  cmapM = cmapMFromIcmapM
instance IdxCTraversable (M i j) where
  icmapM = _icmapM
instance (Prim a, Pretty a) => Pretty (M i j a) where
  pretty = _pretty pretty
instance (Prim a, Show a) => Show (M i j a) where
  show = showPretty . _pretty prettyFromShow
