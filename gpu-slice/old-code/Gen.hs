{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Gen where

import System.Random
import Control.Monad.State
import Data.Array.Accelerate (Acc, Exp, Int32)
import qualified Data.Array.Accelerate as A

prgenGen :: (Exp Int32 -> Exp Int32) -> Exp Int32 -> Exp Int32 -> Exp Int32 -> Exp Int32
prgenGen m a c x = m (a * x + c)

modulus :: Exp Int32 -> Exp Int32
modulus x = A.clearBit x 31

prgen :: Exp Int32 -> Exp Int32
prgen = prgenGen modulus multiplier increment

multiplier :: Exp Int32
multiplier = 1103515245

increment :: Exp Int32
increment = 12345

maxGen :: Exp Int32
maxGen = modulus (-1)

sampleStdUnif :: State (Exp Int32) (Exp Float)
sampleStdUnif = do
  modify prgen
  s <- get
  return $ A.fromIntegral (A.lift s) / A.fromIntegral maxGen

sampleRange :: Exp Float -> Exp Float -> State (Exp Int32) (Exp Float)
sampleRange low high = do
  u <- sampleStdUnif
  return $ low + u * (high - low)

sampleDiscrete :: Exp Int -> Exp Int -> State (Exp Int32) (Exp Int)
sampleDiscrete low high = do
  liftM A.floor $ sampleRange (A.fromIntegral low) (A.fromIntegral high)

evalGen :: State (Exp Int32) a -> a
evalGen = flip evalState 0

evalGenIO :: State (Exp Int32) a -> IO a
evalGenIO aM = do
  liftM (evalState aM . A.lift) (randomIO :: IO Int32)

whileM :: forall a s. (A.Elt a, A.Elt s) => (Exp a -> Exp Bool) -> (Exp a -> State (Exp s) (Exp a)) -> Exp a -> State (Exp s) (Exp a)
whileM test action x0 = state $ \ s ->
  let test' (A.unlift -> (a::Exp a, s::Exp s)) = test a
      action' (A.unlift -> (a::Exp a, s::Exp s)) = A.lift $ runState (action a) s
      x0' = A.lift (x0::Exp a, s::Exp s)
  in A.unlift $ A.while test' action' x0'

