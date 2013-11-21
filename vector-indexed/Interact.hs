module Main where

import Prelude ()
import FP
import Data.Int.Indexed
import qualified Data.V.Generic as V
import Data.V.Boxed (V)
import qualified Text.Pretty.Generic as P

v1 :: V 10 Int
v1 = buildDep sint f
  where
    f :: forall j m. (BInt j -> m Int) -> SInt j -> j < 10 -> Int
    f access
