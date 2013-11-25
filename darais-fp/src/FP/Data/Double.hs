module FP.Data.Double where

import Prelude ()

deqvEpsilon :: Double
deqvEpsilon = 1e-8

deqv :: Double -> Double -> Bool
deqv x y = abs (x - y) < deqvEpsilon

