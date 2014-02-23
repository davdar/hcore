module FP.Classes.Sequence2D where

import Prelude ()
import FP.PrePrelude
import FP.Data.Stream
import FP.Classes.Compat

-- This should really be ND
class Sequence2D t where
  toRowStreams :: (Compat t a) => t a -> Stream (Stream a)
  toColStreams :: (Compat t a) => t a -> Stream (Stream a)
  fromRowStreams :: (Compat t a) => Int -> Int -> Stream (Stream a) -> t a
  fromColStreams :: (Compat t a) => Int -> Int -> Stream (Stream a) -> t a
  rows :: (Compat t a) => t a -> Int
  cols :: (Compat t a) => t a -> Int
  (!!) :: (Compat t a) => t a -> (Int, Int) -> a

