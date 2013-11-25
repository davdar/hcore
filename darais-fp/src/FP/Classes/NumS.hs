module FP.Classes.NumS where

import Prelude ()

type family Zero :: k

type family Succ (i::k) :: k

infixl 6 ^+^
type family (^+^) (i::k) (j::k) :: k

infixl 7 ^*^
type family (^*^) (i::k) (j::k) :: k

data PeanoCaseS (t::k -> *) (i::k) where
  CaseZeroS :: PeanoCaseS t Zero
  CaseSuccS :: t i -> PeanoCaseS t (Succ i)

class PeanoS (t::k -> *) where
  zeroS :: t Zero
  succS :: t i -> t (Succ i)
  peanoCaseS :: t i -> PeanoCaseS t i

infixl 6 |+|
class AdditiveS (t::k -> *) where
  (|+|) :: t i -> t j -> t (i ^+^ j)

infixl 7 |*|
class MultiplicativeS (t::k -> *) where
  (|*|) :: t i -> t j -> t (i ^*^ j)
