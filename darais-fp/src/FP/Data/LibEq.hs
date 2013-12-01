module FP.Data.LibEq where

import Prelude ()
import Unsafe.Coerce

infix 1 :=: 
data (:=:) :: k -> k -> * where
  EqRefl :: a :=: a

unsafeLibEq :: a :=: b
unsafeLibEq = unsafeCoerce EqRefl

withLibEq :: a :=: b -> ((a ~ b) => c) -> c
withLibEq EqRefl f = f
