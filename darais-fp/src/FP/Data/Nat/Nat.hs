{-# LANGUAGE UndecidableInstances #-}

module FP.Data.Nat.Nat 
  ( module FP.Data.Nat.Nat
  , module GHC.TypeLits
  ) where

import GHC.TypeLits (Nat, type (+), type (*))
import FP.Classes.SNum

type instance Zero = (0 :: Nat)
type instance Succ (i::Nat) = i + 1
type instance (^+^) (i::Nat) (j::Nat) = i + j
type instance (^*^) (i::Nat) (j::Nat) = i * j
