module FP.Data.SVector.Storable (SVectorStorable) where

import Prelude ()
import FP.Data.Nat
import FP.Data.Vector.Storable
import FP.Classes.Eq
import FP.Classes.Ord

newtype SVectorStorable (i::Nat) a = SVectorStorable { unSVectorStorable :: VectorStorable a }
  deriving (Eq, Ord)

svectorStorable :: Proxy (SVectorStorable i a)
svectorStorable = proxy
