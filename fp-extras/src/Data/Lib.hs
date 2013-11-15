{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Lib where

import Unsafe.Coerce

data (:=:) :: k -> k -> * where
  EqRefl :: a :=: a

unsafeEqRefl :: a :=: b
unsafeEqRefl = unsafeCoerce EqRefl

withEqRefl :: a :=: b -> ((a ~ b) => c) -> c
withEqRefl EqRefl f = f

