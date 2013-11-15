{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Int.Indexed.BInt
  ( BInt, bump
  ) where

import Prelude ()
import FP
import GHC.TypeLits
import Classes.NumI

newtype BInt (i::Nat) = BInt { unBInt :: Int }
  deriving (Eq, Ord)

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
