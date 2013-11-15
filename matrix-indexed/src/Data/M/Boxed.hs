{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.M.Boxed where

import Prelude ()
import FP
import Data.V.Boxed (V)
import Data.M.Generic
import Data.Int.Indexed

data M i j a = M 
  { mRows :: {-# UNPACK #-} !(SInt i)
  , mCols :: {-# UNPACK #-} !(SInt j)
  , mData :: {-# UNPACK #-} !(V (i*j) a)
  }

instance IMatrix M a where
  type FlatM M = V
  rollM = M
  unrollM = mData
  rowsM = mRows
  colsM = mCols
instance Iterable (M i j a) where
  type Elem (M i j a) = a
  iterOnL = iterOnLFromIiter
  iterOnR = iterOnRFromIiter
instance IdxIterable (M i j a) where
  type Index (M i j a) = (BInt i, BInt j)
  iiterOnL = _iiterOnL
  iiterOnR = _iiterOnR
  (!) = _index
instance CFunctor (M i j) where
  type Compat (M i j) = Universal
  cmap = cmapFromIcmap
instance IdxCFunctor (M i j) where
  type FIndex (M i j) = (BInt i, BInt j)
  icmap = icmapFromIcmapM
instance CTraversable (M i j) where
  cmapM = cmapMFromIcmapM
instance IdxCTraversable (M i j) where
  icmapM = _icmapM
instance (Pretty a) => Pretty (M i j a) where
  pretty = _pretty pretty
instance (Show a) => Show (M i j a) where
  show = showPretty . _pretty prettyFromShow
