module FP.Data.SInt
  ( SInt, sintT, sint
  , CompareS(..), compareS
  , plusIdentityL, plusIdentityR, timesIdentityL, timesIdentityR
  ) where

import Prelude ()
import FP.PrePrelude
import FP.Data.LibEq
import FP.Classes.Static
import FP.Classes.SNum
import FP.Data.Nat
import Foreign.Storable
import FP.Data.Proxy
import GHC.TypeLits

newtype SInt (i::Nat) = SInt { unSInt :: Int }
  deriving (Eq, Ord, Storable)
instance Static SInt where
  type Stripped SInt = Int
  stripS = unSInt
  unsafeS = SInt
instance SPeanoLike SInt where
  zeroS = _zero
  succS = _succ
  peanoCaseS = _peanoCase
instance SAdditive SInt where
  (|+|) = _plus
instance SMultiplicative SInt where
  (|*|) = _times

sintT :: Proxy (SInt i)
sintT = proxy

-- Introduction
sint :: forall (i::Nat). (SingI i) => SInt i
sint = unsafeS $ fromIntegral $ fromSing (sing :: Sing i)

-- Elimination
_peanoCase :: forall i. SInt i -> SPeanoCase SInt i
_peanoCase (SInt i)
  | i < 0 = error "SInt less than zero"
  | i == 0  = withLibEq (unsafeLibEq :: i :=: 0) ZeroS
  | otherwise = withLibEq (unsafeLibEq :: i :=: (0+1)) $ SuccS (unsafeS (i-1) :: SInt 0)

-- Combination
_zero :: SInt 0
_zero = unsafeS 0

_succ :: SInt i -> SInt (i+1)
_succ = unsafeS . succ . stripS

_plus :: SInt i -> SInt j -> SInt (i+j)
_plus x y = unsafeS $ stripS x + stripS y

_times :: SInt i -> SInt j -> SInt (i*j)
_times x y = unsafeS $ stripS x * stripS y

data CompareS i j where
  LtS :: i < j -> CompareS i j
  EqS :: (i ~ j) => CompareS i j
  GtS :: j < i -> CompareS i j

compareS :: forall i j. SInt i -> SInt j -> CompareS i j
compareS iS jS = case compare (stripS iS) (stripS jS) of
  LT -> LtS unsafeLtAxiom
  EQ -> withLibEq (unsafeLibEq :: i :=: j) EqS
  GT -> GtS unsafeLtAxiom

-- Num Axioms
plusIdentityL :: i :=: (0 + i)
plusIdentityL = unsafeLibEq

plusIdentityR :: i :=: (i + 0)
plusIdentityR = unsafeLibEq

timesIdentityL :: i :=: (1 * i)
timesIdentityL = unsafeLibEq

timesIdentityR :: i :=: (i * 1)
timesIdentityR = unsafeLibEq

