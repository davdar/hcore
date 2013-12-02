module GenData where

import System.Process
import Main hiding (main)
import Control.Monad
import System.Environment

main :: IO ()
main = do
  [n] <- liftM (map read) getArgs
  genData n

bench :: IO ()
bench = do
  forM_ [5] $ \ i -> do
    genData (10 ^ i)
    system $ "rm s" ++ show i ++ ".txt"
    system $ "rm p" ++ show i ++ ".txt"
    system $ "rm c" ++ show i ++ ".txt"
    forM_ [1..5] $ \ _ -> do
      system $ "(time ./Main s) 2>> s" ++ show i ++ ".txt"
      system $ "(time ./Main p) 2>> p" ++ show i ++ ".txt"
      system $ "(time ./Main c) 2>> c" ++ show i ++ ".txt"
