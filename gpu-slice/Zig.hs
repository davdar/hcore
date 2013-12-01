{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zig where

import Text.Printf
import Debug.Trace
import Gen
import Control.Arrow
import Control.Monad.State
import Data.Array.Repa.Eval as R
import qualified Data.Array.Repa as R
import Data.Array.Accelerate (Acc, Scalar, Array, Exp, Int32)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as A
import Numeric.SpecFunctions

type RArray = R.Array A.A

i1 :: Int -> R.DIM1
i1 i = R.Z R.:. i

fromList' :: [Float] -> RArray R.DIM1 Float
fromList' xs = R.fromList (i1 $ length xs) xs

tryBuildZig :: (Float -> Float) -> (Float -> Float) -> (Float -> Float) -> Int -> Float -> Float -> (RArray R.DIM1 Float, RArray R.DIM1 Float)
tryBuildZig pdf pdfInv tailArea layers = loop
  where 
    loop low high = 
      if abs (high - low) < 1e-9
        then error "couldn't build zig"
        else
          let x1 = low + (high - low) / 2
              area = tailArea x1
              y1 = pdf x1
              (xs, ys) = 
                (fromList' *** fromList') $ 
                unzip $ 
                (:) (1/0, 0) $
                flip evalState y1 $
                replicateM (layers - 1) $
                action area
              topTarget = pdf 0
              topTable = ys R.! (i1 $ layers - 1)
              xNan = R.foldAllS (||) False $ R.map isNaN xs
          in
          if abs (topTable - topTarget) < eps
            then (xs, ys)
            else if (topTable > topTarget) || xNan
              then loop x1 high
              else loop low x1
      where
        action area = do
          yi <- get
          let xi = pdfInv yi
          put $ yi + area / xi
          return (xi, yi)
        eps = 1e-6

pdfNormalUnnormalized :: Float -> Float
pdfNormalUnnormalized x = exp $ negate $ x * x / 2

pdfNormalUnnormalizedE :: Exp Float -> Exp Float
pdfNormalUnnormalizedE x = exp $ negate $ x * x / 2

inversePdfNormalUnnormalized :: Float -> Float
inversePdfNormalUnnormalized y = sqrt $ negate (log y) * 2

areaOfTailNormalUnnormalized :: Float -> Float
areaOfTailNormalUnnormalized x = realToFrac $ sqrt (pi / 2) * erfc (realToFrac $ x / sqrt 2)

sampleTailFallbackNormal :: Exp Float -> State (Exp Int32) (Exp (Float, Bool))
sampleTailFallbackNormal x1 = do
  u1 <- sampleStdUnif
  u2 <- sampleStdUnif
  let x = negate $ log u1 / x1
      y = negate $ log u2
  return $
    A.cond ((2 * y) A.>* (x * x)) (A.lift (x + x1, True)) $
    A.lift (0::Float, False)

buildZigNormal :: Int -> (RArray R.DIM1 Float, RArray R.DIM1 Float)
buildZigNormal layers = tryBuildZig pdfNormalUnnormalized inversePdfNormalUnnormalized areaOfTailNormalUnnormalized layers 0 20

defaultZigLevels :: Int
defaultZigLevels = 256

tryZig :: 
  Acc (Array A.DIM1 Float) 
  -> Acc (Array A.DIM1 Float) 
  -> (Exp Float -> State (Exp Int32) (Exp (Float, Bool))) 
  -> (Exp Float -> Exp Float)
  -> State (Exp Int32) (Exp (Float, Bool))
tryZig xs ys fallback pdf = do
  layer <- sampleDiscrete 0 ((A.unindex1 $ A.shape xs) - 1)
  let x1 = xs A.! A.index1 1
  u0 <- sampleStdUnif
  u1 <- sampleStdUnif
  xF <- fallback x1
  return $ 
    A.cond (layer A.==* 0) xF $
    let xL = xs A.! (A.index1 layer)
        xNextL = xs A.! A.index1 (layer + 1)
        yL = ys A.! (A.index1 layer)
        yNextL = ys A.! A.index1 (layer + 1)
        x = u0 * xL
    in 
    A.cond (x A.<* xNextL) (A.lift (x, True)) $
    let y = yL + u1 * yNextL
    in 
    A.cond (y A.<* pdf x) (A.lift (x, True)) $
    A.lift (0::Float, False)

zig :: 
  RArray R.DIM1 Float 
  -> RArray R.DIM1 Float 
  -> (Exp Float -> State (Exp Int32) (Exp (Float, Bool)))
  -> (Exp Float -> Exp Float) 
  -> State (Exp Int32) (Exp Float)
zig xs ys fallback pdf = do
    result <- liftM extract $ whileM isNotSuccess loop i0
    u <- sampleStdUnif
    return $ A.cond (u A.<* 0.5) result $ negate result
  where
    extract :: Exp (Float, Bool) -> Exp Float
    extract (A.unlift -> (result, _::Exp Bool)) = result
    isNotSuccess :: Exp (Float, Bool) -> Exp Bool
    isNotSuccess (A.unlift -> (_::Exp Float, success)) = A.not success
    loop :: Exp (Float, Bool) -> State (Exp Int32) (Exp (Float, Bool))
    loop (A.unlift -> (_::Exp Float, _::Exp Bool)) =
      tryZig (A.use $ A.fromRepa xs) (A.use $ A.fromRepa ys) fallback pdf
    i0 :: Exp (Float, Bool)
    i0 = A.lift (0::Float, False)

zigTableNormalDefault :: (RArray R.DIM1 Float, RArray R.DIM1 Float)
zigTableNormalDefault = buildZigNormal defaultZigLevels

zigNormal :: Int -> State (Exp Int32) (Exp Float)
zigNormal levels = uncurry zig (buildZigNormal levels) sampleTailFallbackNormal pdfNormalUnnormalizedE

zigNormalDefault :: State (Exp Int32) (Exp Float)
zigNormalDefault = uncurry zig zigTableNormalDefault sampleTailFallbackNormal pdfNormalUnnormalizedE

-- xs :: Acc (Array A.DIM1 Float)
-- xs = A.use $ A.fromRepa $ fst zigTableNormalDefault
-- 
-- ys :: Acc (Array A.DIM1 Float)
-- ys = A.use $ A.fromRepa $ snd zigTableNormalDefault
