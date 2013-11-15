{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Nat.Lte where

import GHC.TypeLits hiding (type (<=))

infix 8 <=
infix 8 <
data (<=) (i::Nat) (j::Nat) = LteAxiom
type (<) i j = (i+1) <= j

unsafeLteAxiom :: i <= j
unsafeLteAxiom = LteAxiom

lteRefl :: i <= i
lteRefl = LteAxiom

lteZero :: 0 <= i
lteZero = LteAxiom

lteBump :: i <= j -> i+1 <= j+1
lteBump _ = LteAxiom

lteSucc :: i <= i+1
lteSucc = LteAxiom

lteTrans :: i <= j -> j <= k -> i <= k
lteTrans _ _ = LteAxiom

ltSucc :: i < i+1
ltSucc = LteAxiom

ltTrans :: i < j -> j < k -> i < k
ltTrans _ _ = LteAxiom

ltTransLte :: i < j -> j <= k -> i < k
ltTransLte _ _ = LteAxiom
