module FP.Classes.Static where

import Prelude ()
import FP.PrePrelude

class Static (t::k -> *) where
  type Stripped t :: *
  stripS :: t i -> Stripped t
  unsafeS :: Stripped t -> t i

unsafeConvertS :: (Static t) => t i -> t j
unsafeConvertS = unsafeS . stripS

class Static1 (t::k -> * -> *) where
  type Stripped1 t :: * -> *
  stripS1 :: t i a -> Stripped1 t a
  unsafeS1 :: Stripped1 t a -> t i a

class Static2 (t::k1 -> k2 -> * ->  *) where
  type Stripped2 t :: * -> *
  stripS2 :: t i j a -> Stripped2 t a
  unsafeS2 :: Stripped2 t a -> t i j a 
