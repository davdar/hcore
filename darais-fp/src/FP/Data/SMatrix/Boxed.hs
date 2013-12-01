module FP.Data.SMatrix.Boxed where

-- import Prelude ()
-- import FP.Data.SInt
-- import FP.Data.Proxy
-- import FP.Data.SVector.Boxed
-- import FP.Data.Nat
-- 
-- data SMatrix i j a = SMatrix
--   { smatrixRows :: {-# UNPACK #-} !(SInt i)
--   , smatrixCols :: {-# UNPACK #-} !(SInt j)
--   , smatrixData :: {-# UNPACK #-} !(SVector (i*j) a)
--   }
-- 
-- smatrixT :: Proxy (SMatrix i j a)
-- smatrixT = proxy
