module FPUtil.Double where

deqvEpsilon :: Double
deqvEpsilon = 1e-8

deqv :: Double -> Double -> Bool
deqv x y = abs (x - y) < deqvEpsilon
