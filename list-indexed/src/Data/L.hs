{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.L
  ( L, unsafe, strip
  ) where

import GHC.TypeLits

newtype L (i::Nat) a = L { unL :: [a] }
  deriving (Eq, Ord)

unsafe :: [a] -> L i a
unsafe = L

strip :: L i a -> [a]
strip = unL
