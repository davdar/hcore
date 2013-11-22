{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Data.L
  ( L, stripL, unsafeL, exL
  ) where

import Prelude ()
import FP
import Data.Int.Indexed

newtype L (i::Nat) a = L { unL :: [a] }
  deriving (Eq, Ord)

stripL :: L i a -> [a]
stripL = unL

unsafeL :: [a] -> L i a
unsafeL = L

exL :: [a] -> (forall i. L i a -> b) -> b
exL l k = k $ unsafeL l
