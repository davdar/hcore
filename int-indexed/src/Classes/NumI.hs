{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Classes.NumI where

import GHC.TypeLits

type family Zero :: k
type instance Zero = (0 :: Nat)

type family Succ (i::k) :: k
type instance Succ (i::Nat) = i + 1

type family (^+) (i::k) (j::k) :: k
type instance (^+) (i::Nat) (j::Nat) = i + j

type family (^*) (i::k) (j::k) :: k
type instance (^*) (i::Nat) (j::Nat) = i * j

data PeanoCaseI t i where
  CaseZeroI :: PeanoCaseI t 0
  CaseSuccI :: t j -> PeanoCaseI t (j ^+ 1)

class PeanoI t where
  zeroI :: t Zero
  succI :: t i -> t (Succ i)
  peanoCaseI :: t i -> PeanoCaseI t i

class AdditiveI t where
  (|+|) :: t i -> t j -> t (i ^+ j)

class MultiplicativeI t where
  (|*|) :: t i -> t j -> t (i ^* j)
