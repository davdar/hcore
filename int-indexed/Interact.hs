{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude ()
import FP
import Data.Int.Indexed

v :: Int
v = iterDoL (sint :: SInt 5) 0 $ \ (x::BInt 5) -> 
  bintElim x g
  where
    g :: forall k. SInt k -> k < 5 -> Int -> Int
    g kS p =
      let foo :: k < 5
          foo = ltTransLte p lteRefl
      in (+ 1)
      -- this doesn't work if (x < y) is defined as (x+1 <= y) !!!!
