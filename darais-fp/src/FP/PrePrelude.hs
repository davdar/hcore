module FP.PrePrelude
  ( module Prelude
  , module Text.Printf
  , module Control.Category
  , module Data.Maybe
  , module Control.Arrow
  , aloop, aleft, aright
  ) where

import Prelude hiding ((.), id, map, sequence, mapM, length, (!!))
import Control.Category
import Text.Printf
import Data.Maybe
import Control.Arrow hiding (loop, left, right)
import qualified Control.Arrow

aloop :: (ArrowLoop t) => t (a,c) (b,c) -> t a b
aloop = Control.Arrow.loop

aleft :: (ArrowChoice t) => t a b -> t (Either a c) (Either b c)
aleft = Control.Arrow.left

aright :: (ArrowChoice t) => t a b -> t (Either c a) (Either c b)
aright = Control.Arrow.right
