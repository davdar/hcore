{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.L
  ( L, stripL, unsafeL
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
