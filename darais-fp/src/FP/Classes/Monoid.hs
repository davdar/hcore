module FP.Classes.Monoid
  ( module FP.Classes.Monoid
  , module Data.Monoid
  ) where

import Prelude ()
import FP.PrePrelude
import Data.Monoid

appToEndo :: a -> Endo a -> a
appToEndo = flip appEndo

