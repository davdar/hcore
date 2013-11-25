module FP.Classes.Functor
  ( module Data.Functor
  , module FP.Classes.Functor
  , module Prelude
  ) where

import Prelude ()
import Prelude (Functor(..))
import FP.Data.Function

map :: (Functor t) => (a -> b) -> t a -> t b
map = fmap

each :: (Functor t) => t a -> (a -> b) -> t b
each = flip fmap
