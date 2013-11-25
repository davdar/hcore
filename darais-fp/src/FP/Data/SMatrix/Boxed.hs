module FP.Data.SMatrix.Boxed where

import Prelude ()
import FP.Data.SMatrix.Classes

data SMatrix i j a = SMatrix
  { smatrixData :: {-# UNPACK #-} !(SVector (i*j) a)
  , smatrixRows :: {-# UNPACK #-} !(SInt i)
  , smatrixCols :: {-# UNPACK #-} !(SInt j)
  }

smatrix :: Proxy (SMatrix i j a)
smatrix = proxy

instance RolledMatrixS M a where
  type UnrolledMatrixS M = SVector
  rolledMatrixSRoll = SMatrix
  rolledMatrixSUnroll = smatrixData
  rolledMatrixSRows = smatrixRows
  rolledMatrixSCols = smatrixCols
