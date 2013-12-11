{-# LANGUAGE ViewPatterns #-}

module Main where

import Model
import System.Environment

main :: IO ()
main = do
  [read -> n] <- getArgs
  lmainAcc n
