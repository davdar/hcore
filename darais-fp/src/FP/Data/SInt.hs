module FP.Data.SInt
  ( SInt, sint, sintEx
  , ICompare(..), icompare
  , plusIdentity, timesIdentityL, timesIdentityR
  , bint, bintExtend, bintElim
  ) where

import Prelude ()
import FP.Classes.Static
import FP.Classes.NumS
import FP.Pretty

newtype SInt (i::Nat) = SInt { unSInt :: Int }
  deriving (Eq, Ord, Storable)
instance Static SInt where
  type UnStatic SInt = Int
  stripS = unSInt
  unsafeS = SInt
instance PeanoS SInt where
  zeroS = _zero
  succS = _succ
  peanoCaseS = _peanoCaseS
instance AdditiveS SInt where
  (|+|) = _plus
instance MultiplicativeS SInt where
  (|*|) = _times
instance Pretty (SInt i) where
  pretty = _pretty
instance Show (SInt i) where
  show = show'

sint :: Proxy (Sint i)
sint = proxy

-- Introduction
mkSInt :: forall (i::Nat). (SingI i) => SInt i
mkSInt = unsafeI $ fromIntegral $ fromSing (sing :: Sing i)

sintEx :: Int -> Ex SInt
sintEx = Ex . unsafeI

-- Elimination
_peanoCaseI :: SInt i -> PeanoCaseI SInt i
_peanoCaseI (SInt i)
  | i < 0 = error "SInt less than zero"
  | i == 0  = unsafeCoerce CaseZeroI
  | otherwise = unsafeCoerce $ CaseSuccI (unsafeI (i-1) :: SInt 0)

_iterOnL :: (BInt i -> a -> a) -> SInt i -> a -> a
_iterOnL f iS = case peanoCaseI iS of
  CaseZeroI -> id
  CaseSuccI iS' -> f (bint iS' ltSucc) .! _iterOnL (f . bump) iS'

_iterOnR :: (BInt i -> a -> a) -> SInt i -> a -> a
_iterOnR f iS = case peanoCaseI iS of
  CaseZeroI -> id
  CaseSuccI iS' -> _iterOnR (f . bump) iS' . f (bint iS' ltSucc)

-- Combination
_zeroI :: SInt 0
_zeroI = unsafeI 0

_succI :: SInt i -> SInt (i+1)
_succI = unsafeI . succ . stripI

_plus :: SInt i -> SInt j -> SInt (i+j)
_plus x y = unsafeI $ stripI x + stripI y

_times :: SInt i -> SInt j -> SInt (i*j)
_times x y = unsafeI $ stripI x * stripI y

data ICompare i j where
  ILt :: i < j -> ICompare i j
  IEq :: (i ~ j) => ICompare i j
  IGt :: j < i -> ICompare i j

icompare :: forall i j. SInt i -> SInt j -> ICompare i j
icompare iS jS = case compare (stripI iS) (stripI jS) of
  LT -> ILt unsafeLtAxiom
  EQ -> withEqRefl (unsafeEqRefl :: i :=: j) IEq
  GT -> IGt unsafeLtAxiom

-- Printing
_pretty :: (MonadPretty m) => SInt i -> m ()
_pretty = pretty . stripI

-- Num Axioms
plusIdentity :: i :=: (i + 0)
plusIdentity = unsafeEqRefl

timesIdentityL :: i :=: (1 * i)
timesIdentityL = unsafeEqRefl

timesIdentityR :: i :=: (i * 1)
timesIdentityR = unsafeEqRefl

---------- BInt ----------

-- Introduction
bint :: SInt i -> i < j -> BInt j
bint iS _ = unsafeI $ stripI iS

-- TODO: flip arguments
bintExtend :: BInt i -> i <= j -> BInt j
bintExtend iB iLtej = bintElim iB $ \ kS kLti ->
  bint kS $ ltTransLte kLti iLtej

-- Elimination
bintElim :: BInt j -> (forall i. SInt i -> i < j -> b) -> b
bintElim iB k = k (unsafeI $ stripI iB) unsafeLtAxiom

