{-# LANGUAGE ConstraintKinds #-}

module Main where

import ML.Gen
import Data.V.Prim (V, Prim)
import Prelude ()
import FP
import Data.V.Prim (V)
import Data.M.Prim (M)
import qualified Data.V.Generic as V
import qualified Data.M.Generic as M
import qualified Data.M.Prim as M
import qualified Data.V.Prim as VP
import Data.Int.Indexed
import ML.MonadGen
import ML.Statistics
import Statistics.Distribution.Normal
import qualified Numeric.LinearAlgebra as HM
import Vis.Plot


main :: IO ()
main = forLM (sint :: SInt 10) $ \ kB -> do
  when (stripI kB < 1) $ do
    (ys1, ys2) <- execMGen $ do
      ys1 <- sampleKernel (V.fill dim 0) (alphas ! kB) (ls ! kB)
      ys2 <- sampleKernel (V.fill dim 0) (alphas ! kB) (ls ! kB)
      return (ys1, ys2)
    forLM pis $ \ theta -> do
      let ys3 = cos theta &*& ys1 &+& sin theta &*& ys2
          title :: String
          title = printf "a=%f l=%f theta=%f" (alphas ! kB) (ls ! kB) theta
          outFile = printf "step=%d:theta=%.2g" (stripI kB) theta 
      execCPlot defaultPlotEnv $
        localViewMod outFileL (outFile ++) $
        localViewSet titleL title $
        display $ do
          plotLines xs ys1
          plotLines xs ys2
          plotLines xs ys3

dim :: SInt 100
dim = sint

xs :: V 100 Double
xs = grid sint 0 5

pis :: V 10 Double
pis = grid sint (negate pi) pi

alphas :: V 10 Double
alphas = V.iterateN sint 0.5 (+ 1)

ls :: V 10 Double
ls = V.iterateN sint 0.5 (+ 1)

sampleKernel :: (MonadGen m) => V i Double -> Double -> Double -> m (V i Double)
sampleKernel mean alpha l = 
  sampleMVN mean $ M.build (V.length mean) (V.length mean) $ \ (iB, jB) ->
    kernel alpha l (fromIntegral $ stripI iB) 
                   (fromIntegral $ stripI jB)

p11 :: IO ()
p11 = forLM (sint :: SInt 10) $ \ kB -> do
  ys <- execMGen $ 
    sampleKernel (V.fill dim 0) (alphas ! kB) (ls ! kB)
  let title :: String
      title = printf "a=%f l=%f" (alphas ! kB) (ls ! kB)
  execCPlot defaultPlotEnv $ 
    localViewMod outFileL (show (stripI kB) ++) $
    localViewSet titleL title $
    display $
    plotLines xs ys

kernel :: Double -> Double -> Double -> Double -> Double
kernel alpha l x x' = 
  alpha 
  * exp ((-1) / (2 * l ^ 2) * (x - x') ^ 2)
  + eps * delta x x'
  where
    eps = 10e-6
    delta x x' | x == x'   = 1
               | otherwise = 0

grid :: SInt i -> Double -> Double -> V i Double
grid iS low high =
  V.build iS $ \ iB ->
    low + (fromIntegral $ stripI iB) * width + (width / 2)
  where
    range = high - low
    width = range / (fromIntegral $ stripI iS)
    
sampleMVN :: (MonadGen m) => V i Double -> M i i Double -> m (V i Double)
sampleMVN mean covariance = do
  snSamples <- VP.fillM (V.length mean) $ 
    genContVarM $ normalDistr 0 1
  return $ colToVector $ M.colVector mean %+% trans (cholesky covariance) %*% M.colVector snSamples
  where
    cholesky = M.unsafeFromHM . HM.chol . M.toHM

type Works a = (Prim a, HM.Field a, HM.Element a, Num a)
   
infixl 6 %+%
(%+%) :: (Works a) => M i j a -> M i j a -> M i j a
(%+%) m1 m2 = M.build (M.rows m1) (M.cols m1) $ \ idx -> (m1 ! idx) + (m2 ! idx)

infixl 7 %*%
(%*%) :: (Works a) => M i j a -> M j k a -> M i k a
(%*%) m1 m2 = M.build (M.rows m1) (M.cols m2) $ \ (iS, kS) ->
  getRow iS m1 `dotProduct` getCol kS m2

infixl 6 &+&
(&+&) :: (Works a) => V i a -> V i a -> V i a
(&+&) xs ys = V.build (V.length xs) $ \ iS -> (xs ! iS) + (ys ! iS)

infixl 7 &*&
(&*&) :: (Works a) => a -> V i a -> V i a
(&*&) x = cmap $ (*) x

getRow :: (Works a) => BInt i -> M i j a -> V j a
getRow iB m = V.build (M.cols m) $ \ jB -> m ! (iB, jB)

getCol :: (Works a) => BInt j -> M i j a -> V i a
getCol jB m = V.build (M.rows m) $ \ iB -> m ! (iB, jB)

dotProduct :: (Works a) => V i a -> V i a -> a
dotProduct v1 v2 = iterDoL (V.length v1) 0 $ \ iB -> (+) $ (v1 ! iB) * (v2 ! iB)

trans :: (Works a) => M i j a -> M j i a
trans m = M.build (M.cols m) (M.rows m) $ (!) m . swap

diag :: (Works a) => SInt i -> a -> M i i a
diag iS x = M.build iS iS $ \ (iB, jB) ->
  if iB == jB then x else 0

colToVector :: (Works a) => M i 1 a -> V i a
colToVector m = V.build (M.rows m) $ \ iS -> m ! (iS, unsafeI 0)
