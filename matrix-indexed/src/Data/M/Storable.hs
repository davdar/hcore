{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.M.Storable where

import Unsafe.Coerce
import Prelude ()
import FP
import Data.V.Storable (V, Storable)
import Data.M.Generic
import Data.Int.Indexed
import Data.Packed.Matrix (Matrix, Element)
import Foreign.Storable
import qualified Data.Packed.Matrix as HM
import qualified Numeric.LinearAlgebra.Algorithms as HM
import qualified Data.V.Generic as V

data M i j a = M 
  { mRows :: {-# UNPACK #-} !(SInt i)
  , mCols :: {-# UNPACK #-} !(SInt j)
  , mData :: {-# UNPACK #-} !(V (i*j) a)
  }

instance (Storable a) => IMatrix M a where
  type FlatM M = V
  rollM = M
  unrollM = mData
  rowsM = mRows
  colsM = mCols
instance (Storable a) => Iterable (M i j a) where
  type Elem (M i j a) = a
  iterOnL = iterOnLFromIiter
  iterOnR = iterOnRFromIiter
instance (Storable a) => IdxIterable (M i j a) where
  type Index (M i j a) = (BInt i, BInt j)
  iiterOnL = _iiterOnL
  iiterOnR = _iiterOnR
  (!) = _index
instance CFunctor (M i j) where
  type Compat (M i j) = Storable
  cmap = cmapFromIcmap
instance IdxCFunctor (M i j) where
  type FIndex (M i j) = (BInt i, BInt j)
  icmap = icmapFromIcmapM
instance CTraversable (M i j) where
  cmapM = cmapMFromIcmapM
instance IdxCTraversable (M i j) where
  icmapM = _icmapM
instance (Storable a, Pretty a) => Pretty (M i j a) where
  pretty = _pretty pretty
instance (Storable a, Show a) => Show (M i j a) where
  show = showPretty . _pretty prettyFromShow

toHM :: (Element a) => M i j a -> Matrix a
toHM m = HM.reshape (stripI $ cols m) $ V.stripIV (unrollM m)

fromHM :: (Element a) => Matrix a -> (forall i j. M i j a -> b) -> b
fromHM m k = k $ rollM (unsafeI $ HM.rows m) (unsafeI $ HM.cols m) $ V.unsafeIV $ HM.flatten m

unsafeFromHM :: forall i j a. (Element a) => Matrix a -> M i j a
unsafeFromHM m = fromHM m (unsafeCoerce :: forall i i' j j'. M i j a -> M i' j' a)

trans :: (Element a) => M i j a -> M j i a
trans = unsafeFromHM . HM.trans . toHM

inv :: M i j Double -> M i j Double
inv = unsafeFromHM . HM.inv . toHM

choleskyM :: M i j Double -> Maybe (M i j Double)
choleskyM = liftM unsafeFromHM . HM.mbCholSH . toHM

cholesky :: M i j Double -> M i j Double
cholesky m = case choleskyM m of
  Nothing -> error "cholesky on non-positive-definite matrix"
  Just x -> x

det :: M i i Double -> Double
det = HM.det . toHM
