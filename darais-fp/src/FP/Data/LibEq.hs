module FP.Data.LibEq where

import Prelude ()

data (:=:) :: k -> k -> * where
  EqRefl :: a :=: a

unsafeLibEq :: a :=: b
unsafeLibEq = unsafeCoerce EqRefl

withLibEq :: a :=: b -> ((a ~ b) => c) -> c
withLibEq EqRefl f = f

withUnsafeLibEqP :: Proxy a -> Proxy b -> ((a ~ b) => c) -> c
withUnsafeLibEqP = undefined

