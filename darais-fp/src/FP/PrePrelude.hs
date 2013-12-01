module FP.PrePrelude
  ( module Prelude
  , module Text.Printf
  , module Control.Category
  ) where

import Prelude hiding ((.), id, map, sequence, mapM, length, (!!))
import Control.Category
import Text.Printf
