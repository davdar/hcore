module FP.Data.Function
  ( module FP.Data.Function
  , module Data.Function
  ) where

import Prelude ()
import FP.PrePrelude
import Data.Function hiding ((.), id)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(..:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(..:) = (.) . (.:)

(.!) :: (b -> c) -> (a -> b) -> a -> c
(.!) g f x = g $! f x

applyTo :: a -> (a -> b) -> b
applyTo = flip id


