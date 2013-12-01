{-# LANGUAGE UndecidableInstances #-}

module FP.Data.Peano where

import FP.Classes.SNum
import FP.Data.LibEq
import FP.Data.Proxy

data Peano = Z | S Peano

type instance Zero = (Z :: Peano)
type instance Succ (n::Peano) = S n
type instance (^+^) Z n = n
type instance (^+^) (S n) m = n ^+^ S m
type instance (^*^) Z n = Z
type instance (^*^) (S n) m = n ^+^ n ^*^ m

data SPeano n where
  SZ :: SPeano Z
  SS :: (IPeano n) => Proxy n -> SPeano (S n)

class IPeano n where
  iPeano :: SPeano n

instance IPeano Z where
  iPeano = SZ
instance (IPeano n) => IPeano (S n) where
  iPeano = SS proxy
