{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Data.ExI where

import Text.Pretty

data ExI (p::k -> * -> *) a where
  ExI :: forall p i a. p i a -> ExI p a

class PrettyExI p where
  prettyExI :: (MonadPretty m, Pretty a) => p i a -> m ()

unExI :: ExI p a -> (forall i. p i a -> b) -> b
unExI (ExI x) f = f x

instance (PrettyExI p, Pretty a) => Pretty (ExI p a) where
  pretty xE = unExI xE prettyExI