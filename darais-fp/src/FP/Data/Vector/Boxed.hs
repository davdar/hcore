module FP.Data.Vector.Boxed 
  ( Vector
  ) where

import Prelude ()
import FP.Prelude
import Data.Vector (Vector)
import qualified Data.Vector as Vector

instance CFunctor Vector where
  Compat Vector = Universal
  cmap = map
