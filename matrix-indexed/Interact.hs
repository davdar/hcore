module Main where

import Prelude ()
import FP
import qualified Data.M.Generic as M
import qualified Data.M.Storable as M
import Data.M.Storable (M)
import qualified Data.V.Generic as V
import Data.V.Boxed (V)
import Data.Int.Indexed
import qualified Text.Pretty.Generic as P
import qualified Data.L as L

m1 :: M 3 3 Double
m1 = M.fromLRows sint sint $ L.unsafeL
  [ 3, 2, 1
  , 2, 9, 0
  , 1, 0, 4
  ]
