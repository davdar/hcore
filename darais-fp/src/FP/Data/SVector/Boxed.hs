module FP.Data.SVector.Boxed (SVectorBoxed) where

import Prelude ()
import FP.Data.Nat
import FP.Data.Vector.Boxed

newtype SVector (i::Nat) a = SVector { unSVectorBoxed :: Vector a }
  deriving (Eq, Ord)

svector :: Proxy (SVector i a)
svector = proxy
