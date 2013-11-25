module FP.Classes.Sequence2D where

import Prelude ()

class (CFunctor t) => Sequence2D t where
  toRowStreams :: (Compat t a) => t a -> Stream (Stream a)
  toColStreams :: (Compat t a) => t a -> Stream (Stream a)
  fromRowStreams :: (Compat t a) => Int -> Int -> Stream (Stream a) -> t a
  fromColStreams :: (Compat t a) => Int -> Int -> Stream (Steram a) -> t a
  (!!&) :: (Compat t a) => t a -> (Int, Int) -> a

class 
  ( Static2 t
  , CFunctor t
  , CFunctor (UnStatic2 t)
  , Compat t ~ Compat (UnStatic2 t)
  , Sequence2D (UnStatic2 t)
  ) => SSequence2D where
    toRowStreamsS :: (Compat (t i j) a) => t i j a -> SStream i (SStream j a)
    toColStreamsS :: (Compat (t i j) a) => t i j a -> SStream j (SStream i a)
    fromRowStreamsS :: (Compat (t i j) a) => SInt i -> SInt j -> SStream i (SStream j a)
    fromColStreamsS :: (Compat (t i j) a) => SInt i -> SInt j -> SStream j (SStream i a)
    (!!) :: (Compat t a) => t a -> (SInt i, SInt j) -> a

