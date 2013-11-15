{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Int.Indexed.SInt
  ( SInt, sint, sintEx
  , plusIdentity, timesIdentityL, timesIdentityR
  , bint
  ) where

import Prelude ()
import FP
import Data.Nat.Lte
import Classes.NumI
import Unsafe.Coerce
import GHC.TypeLits
import Data.Int.Indexed.BInt

newtype SInt (i::Nat) = SInt { unSInt :: Int }
  deriving (Eq, Ord)
instance Indexed SInt where
  type UnIndexed SInt = Int
  stripI = unSInt
  unsafeI = SInt
instance PeanoI SInt where
  zeroI = _zeroI
  succI = _succI
  peanoCaseI = _peanoCaseI
instance AdditiveI SInt where
  (|+|) = _plus
instance MultiplicativeI SInt where
  (|*|) = _times
instance Iterable (SInt i) where
  type Elem (SInt i) = BInt i
  iterOnL = _iterOnL
  iterOnR = _iterOnR
instance Pretty (SInt i) where
  pretty = _pretty

-- Introduction
sint :: forall (i::Nat). (SingI i) => SInt i
sint = unsafeI $ fromIntegral $ fromSing (sing :: Sing i)

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
  CaseSuccI iS' -> f (bint iS' lteRefl) .! _iterOnL (f . bump) iS'

_iterOnR :: (BInt i -> a -> a) -> SInt i -> a -> a
_iterOnR f iS = case peanoCaseI iS of
  CaseZeroI -> id
  CaseSuccI iS' -> _iterOnR (f . bump) iS' . f (bint iS' lteRefl)

-- Combination
_zeroI :: SInt 0
_zeroI = unsafeI 0

_succI :: SInt i -> SInt (i+1)
_succI = unsafeI . succ . stripI

_plus :: SInt i -> SInt j -> SInt (i+j)
_plus x y = unsafeI $ stripI x + stripI y

_times :: SInt i -> SInt j -> SInt (i*j)
_times x y = unsafeI $ stripI x * stripI y

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
bint i LteAxiom = unsafeI $ stripI i
