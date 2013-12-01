module FP.Classes.SSequence where

class SSequence t where
  toStreamS :: (Compat (t i) a) => t i a -> SStream i a
  fromStreamS :: (Compat (t i) a) => SInt i -> SStream i a -> t i a
  lengthS :: (Compat (t i) a) => t i a -> SInt i
  (|!|) :: (Compat (t i) a) => t i a -> BInt i -> a
