module FP.Data.SVector.Boxed where -- ( SVector, svectorT) where

-- import Prelude ()
-- import FP.PrePrelude
-- import FP.Classes.Static
-- import FP.Data.Nat
-- import FP.Data.Vector.Boxed
-- import FP.Data.Proxy
-- 
-- newtype SVector (i::Nat) a = SVector { unSVector :: Vector a }
--   deriving (Eq, Ord)
-- 
-- instance Static1 SVector where
--   type Stripped1 SVector = Vector
--   stripS1 = unSVector
--   unsafeS1 = SVector
-- 
-- svectorT :: Proxy (SVector i a)
-- svectorT = proxy
