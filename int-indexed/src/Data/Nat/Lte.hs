{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Nat.Lte where

import GHC.TypeLits hiding (type (<=))

infix 8 <=
infix 8 <
data (<=) (i::Nat) (j::Nat) = LteAxiom
data (<) (i::Nat) (j::Nat) = LtAxiom

unsafeLteAxiom :: i <= j
unsafeLteAxiom = LteAxiom

lteRefl :: i <= i
lteRefl = LteAxiom

lteFromLt :: i < j -> i <= j
lteFromLt _ = LteAxiom

lteZero :: 0 <= i
lteZero = LteAxiom

lteBump :: i <= j -> i+1 <= j+1
lteBump _ = LteAxiom

lteSucc :: i <= i+1
lteSucc = LteAxiom

lteTrans :: i <= j -> j <= k -> i <= k
lteTrans _ _ = LteAxiom

lteFromLtSucc :: i < (j + 1) -> i <= j
lteFromLtSucc _ = LteAxiom

lteSuccFromLt :: i < j -> (i + 1) <= j
lteSuccFromLt _ = LteAxiom

unsafeLtAxiom :: i < j
unsafeLtAxiom = LtAxiom

ltSucc :: i < i+1
ltSucc = LtAxiom

ltTrans :: i < j -> j < k -> i < k
ltTrans _ _ = LtAxiom

ltTransLte :: i < j -> j <= k -> i < k
ltTransLte _ _ = LtAxiom

ltAbsurd :: i < j -> j < i -> a
ltAbsurd _ _ = error "impossible"

ltAbsurdLte :: i < j -> j <= i -> a
ltAbsurdLte _ _ = error "impossible"

lteAbsurdLt :: i <= j -> j < i -> a
lteAbsurdLt _ _ = error "impossible"
