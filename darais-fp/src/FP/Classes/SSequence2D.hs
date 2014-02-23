module FP.Classes.SSequence2D where

import Prelude ()
import FP.Data.BInt
import FP.Data.SStream (SStream)
import FP.Data.SInt
import FP.Classes.Compat

class SSequence2D t where
  toRowStreamsS :: (Compat (t i j) a) => t i j a -> SStream i (SStream j a)
  toColStreamsS :: (Compat (t i j) a) => t i j a -> SStream j (SStream i a)
  fromRowStreamsS :: (Compat (t i j) a) => SInt i -> SInt j -> SStream i (SStream j a) -> t i j a
  fromColStreamsS :: (Compat (t i j) a) => SInt i -> SInt j -> SStream j (SStream i a) -> t i j a
  rowsS :: (Compat (t i j) a) => t i j a -> SInt i
  colsS :: (Compat (t i j) a) => t i j a -> SInt j
  (|!!|) :: (Compat (t i j) a) => t i j a -> (BInt i, BInt j) -> a

