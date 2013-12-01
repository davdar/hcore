module FP.Classes.SNum where

import Prelude ()

type family Zero :: k

type family Succ (i::k) :: k

infixl 6 ^+^
type family (^+^) (i::k) (j::k) :: k

infixl 7 ^*^
type family (^*^) (i::k) (j::k) :: k

data SPeanoCase (t::k -> *) (i::k) where
  ZeroS :: SPeanoCase t Zero
  SuccS :: t i -> SPeanoCase t (Succ i)

class SPeanoLike (t::k -> *) where
  zeroS :: t Zero
  succS :: t i -> t (Succ i)
  peanoCaseS :: t i -> SPeanoCase t i

infixl 6 |+|
class SAdditive (t::k -> *) where
  (|+|) :: t i -> t j -> t (i ^+^ j)

infixl 7 |*|
class SMultiplicative (t::k -> *) where
  (|*|) :: t i -> t j -> t (i ^*^ j)
