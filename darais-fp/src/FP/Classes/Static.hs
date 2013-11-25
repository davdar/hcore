module FP.Classes.Static where

import Prelude ()

class Static (t::k -> *) where
  type UnStatic t :: *
  stripS :: t i -> UnStatic t
  unsafeS :: UnStatic t -> t i

class Static1 (t::k -> * -> *) where
  type UnStatic1 t :: * -> *
  stripS1 :: t i a -> UnStatic1 t a
  unsafeS1 :: UnStatic1 t a -> t i a

class Static2 (t::k1 -> k2 -> * ->  *) where
  type UnStatic2 t :: * -> *
  stripS2 :: t i j a -> UnStatic2 t a
  unsafeS2 :: UnStatic2 t a -> t i j a 
