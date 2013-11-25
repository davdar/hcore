module FP.Data.BInt where

import Prelude ()

newtype BInt (i::Nat) = BInt { unBInt :: Int }
  deriving (Eq, Ord, Storable)
instance Indexed BInt where
  type UnIndexed BInt = Int
  stripI = unBInt
  unsafeI = BInt
instance AdditiveI BInt where
  (|+|) = _plus
instance MultiplicativeI BInt where
  (|*|) = _times
instance Pretty (BInt i) where
  pretty = _pretty
instance Show (BInt i) where
  show = show'

-- Combination
_plus :: BInt i -> BInt j -> BInt (i+j)
_plus iB jB = unsafeI $ stripI iB + stripI jB

_times :: BInt i -> BInt j -> BInt (i*j)
_times iB jB = unsafeI $ stripI iB * stripI jB

-- Proofs
bump :: BInt i -> BInt (i + 1)
bump = unsafeI . stripI

-- Printing
_pretty :: (MonadPretty m) => BInt i -> m ()
_pretty = pretty . stripI

