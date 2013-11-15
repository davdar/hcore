{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.Ex where

import Classes.Indexed
import Text.Pretty

data Ex (p::k -> *) where
  Ex :: forall p i. p i -> Ex p

class PrettyEx p where
  prettyEx :: (MonadPretty m) => p i -> m ()

unEx :: Ex p -> (forall i. p i -> b) -> b
unEx (Ex x) f = f x

stripEx :: (Indexed p) => Ex p -> UnIndexed p
stripEx e = unEx e stripI

instance (PrettyEx p) => Pretty (Ex p) where
  pretty xE = unEx xE prettyEx
