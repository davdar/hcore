module Main where

import Prelude ()
import FP
import Data.Int.Indexed
import Data.V.Prim (V)
import qualified Data.V.Generic as V
import Vis.Plot

d :: V 10 Double
d = V.build sint (fromIntegral . stripI)

main :: IO ()
main = plotit $ plotHist d
