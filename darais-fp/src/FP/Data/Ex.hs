module FP.Data.Ex where

import Prelude ()
import FP.PrePrelude
import FP.Classes.Static
import FP.Pretty

data Ex (p :: k -> *) where
  Ex :: forall p i. p i -> Ex p

unEx :: Ex p -> (forall i. p i -> b) -> b
unEx (Ex x) f = f x

ex :: (Static p) => Stripped p -> Ex p
ex e = Ex $ unsafeS e

stripEx :: (Static p) => Ex p -> Stripped p
stripEx e = unEx e stripS

class PrettyEx p where
  prettyEx :: (MonadPretty m) => p i -> m ()

instance (PrettyEx p) => Pretty (Ex p) where
  pretty xE = unEx xE prettyEx
instance (PrettyEx p) => Show (Ex p) where
  show = show'

data Ex1 (p :: k -> * -> *) (a :: *) where
  Ex1 :: forall p i a. p i a -> Ex1 p a

data Ex01 (p :: k1 -> k2 -> * -> *) (i :: k1) (a :: *) where
  Ex01 :: forall p i j a. p i j a -> Ex01 p i a

data Ex10 (p :: k1 -> k2 -> * -> *) (j :: k2) (a :: *) where
  Ex10 :: forall p i j a. p i j a -> Ex10 p j a

data Ex11 (p :: k1 -> k2 -> * -> *) (a :: *) where
  Ex11 :: forall p i j a. p i j a -> Ex11 p a
