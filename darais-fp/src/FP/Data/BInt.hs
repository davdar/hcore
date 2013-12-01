module FP.Data.BInt where

import FP.Data.SInt
import FP.Data.Function
import Prelude ()
import FP.PrePrelude
import FP.Classes.Static
import FP.Classes.SNum
import FP.Data.Nat
import Foreign.Storable

newtype BInt (i::Nat) = BInt { unBInt :: Int }
  deriving (Eq, Ord, Storable)
instance Static BInt where
  type Stripped BInt = Int
  stripS = unBInt
  unsafeS = BInt
instance SAdditive BInt where
  (|+|) = _plus
instance SMultiplicative BInt where
  (|*|) = _times

-- Combination
_plus :: BInt i -> BInt j -> BInt (i+j)
_plus iB jB = unsafeS $ stripS iB + stripS jB

_times :: BInt i -> BInt j -> BInt (i*j)
_times iB jB = unsafeS $ stripS iB * stripS jB

-- Proofs
bump :: BInt i -> BInt (i + 1)
bump = unsafeS . stripS

---------- BInt ----------

_iterOnL :: (BInt i -> a -> a) -> SInt i -> a -> a
_iterOnL f iS = case peanoCaseS iS of
  ZeroS -> id
  SuccS iS' -> f (bint iS' ltSucc) .! _iterOnL (f . bump) iS'

_iterOnR :: (BInt i -> a -> a) -> SInt i -> a -> a
_iterOnR f iS = case peanoCaseS iS of
  ZeroS -> id
  SuccS iS' -> _iterOnR (f . bump) iS' . f (bint iS' ltSucc)

-- Introduction
bint :: SInt i -> i < j -> BInt j
bint iS _ = unsafeS $ stripS iS

-- TODO: flip arguments
bintExtend :: BInt i -> i <= j -> BInt j
bintExtend iB iLtej = bintElim iB $ \ kS kLti ->
  bint kS $ ltTransLte kLti iLtej

-- Elimination
bintElim :: BInt j -> (forall i. SInt i -> i < j -> b) -> b
bintElim iB k = k (unsafeS $ stripS iB) unsafeLtAxiom

