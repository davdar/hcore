module FP.Data.SMatrix.Classes where

import Prelude ()
import FP.Data.Nat
import FP.Data.SInt

class RolledMatrixS t where
  type UnrolledMatrixS t :: Nat -> * -> *
  rolledMatrixSRoll :: SInt i -> SInt j -> UnrolledMatrixS t (i*j) a -> t i j a
  rolledMatrixSUnroll :: t i j a -> UnrolledMatrixS t (i*j) a
  rolledMatrixSRows :: t i j a -> SInt i
  rolledMatrixSCols :: t i j a -> SInt j
