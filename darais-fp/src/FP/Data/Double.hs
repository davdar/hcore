module FP.Data.Double where

import Prelude ()
import FP.PrePrelude

class ToDouble t where
  double :: t -> Double

instance ToDouble Int where
  double = fromIntegral

deqvEpsilon :: Double
deqvEpsilon = 1e-8

deqv :: Double -> Double -> Bool
deqv x y = abs (x - y) < deqvEpsilon

