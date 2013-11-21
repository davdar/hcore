module Main where

import Prelude ()
import FP
import Data.Int.Indexed
import Data.V.Prim (V)
import qualified Data.V.Generic as V
import Vis.Plot

d :: V 10 Double
d = V.build sint (fromIntegral . stripI)

xs :: V 100 Double
xs = V.grid sint 0 1

ys :: V 100 Double
ys = cmap sin xs

ysMin :: V 100 Double
ysMin = cmap (/ 2) ys

ysMax :: V 100 Double
ysMax = cmap (+ 0.5) ys

histEx :: IO ()
histEx = plotit $ plotHist d

main :: IO ()
main = plotit $ plotLinesError xs ys ysMin ysMax
