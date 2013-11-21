{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Int.Indexed
import Data.M.Storable (M)
import Data.V.Storable (V, Storable)
import FP
import ML.Gen
import ML.MonadGen
import ML.Statistics
import Prelude ()
import Statistics.Distribution.Normal
import System.IO.Unsafe
import Vis.Plot
import qualified Data.M.Generic as M hiding (trans)
import qualified Data.M.Storable as M
import qualified Data.V.Generic as V
import qualified Data.V.Storable as VS
import qualified Data.Vector.Storable as Vector

type Kernel = Double -> Double -> Double

dim :: SInt 100
dim = sint

xs :: V 100 Double
xs = V.grid sint 0 5

hypers :: SInt 4
hypers = sint

pis :: V 10 Double
pis = V.grid sint (negate pi) pi

alphas :: V 4 Double
alphas = V.fill sint 1

ls :: V 4 Double
ls = V.iterateN sint 1 (+ 5)

kernel :: Double -> Double -> Double -> Double -> Double
kernel alpha l x x' = 
  alpha 
  * exp ((-1) / (2 * l ^ 2) * (x - x') ^ 2)
  + eps * delta x x'
  where
    eps = 1e-6
    delta x x' | x == x'   = 1
               | otherwise = 0

sampleMVN :: (MonadGen m) => V i Double -> M i i Double -> m (V i Double)
sampleMVN mean covariance = do
  snSamples <- VS.fillM (V.length mean) $ 
    genContVarM $ normalDistr 0 1
  return $ M.colToVector $ 
    M.colFromVector mean 
    M.%+% M.trans (M.cholesky covariance)
    M.%*% M.colFromVector snSamples

logLikelihoodMVN :: V i Double -> M i i Double -> V i Double -> V i Double -> Double
logLikelihoodMVN mean covariance xs ys =
  (-1 / 2) 
  * 
  M.fromSingleton
  ( M.rowFromVector ys 
    M.%*% covariance 
    M.%*% M.colFromVector ys
  )
  - (1 / 2)
  * log (M.det $ M.trans $ M.cholesky covariance)
  - ((fromIntegral $ stripI $ V.length mean) / 2)
  * log (2 * pi)

buildKernel :: Kernel -> V i Double -> V j Double -> M i j Double
buildKernel k xs xs' = M.build (V.length xs) (V.length xs') $ \ (iS, jS) -> 
  k (xs ! iS) (xs' ! jS)

sampleKernel :: (MonadGen m) => Kernel -> V i Double -> m (V i Double)
sampleKernel k xs = sampleMVN (V.fill (V.length xs) 0) $ buildKernel k xs xs

p11 :: IO ()
p11 = execMGenT $ execCPlotT defaultPlotEnv $ forLM hypers $ \ kB ->
  let title = printf "a=%f l=%f" (alphas ! kB) (ls ! kB)
  in 
  localViewMod outFileL (++ show' kB) $
  localViewSet titleL title $
  display $
    forLM (sint :: SInt 10) $ \ _ -> do
      ys <- sampleKernel (kernel (alphas ! kB) (ls ! kB)) xs
      plotLines xs ys

p12 :: IO ()
p12 = execMGenT $ execCPlotT defaultPlotEnv $ forLM hypers $ \ kB ->
  localViewMod outFileL (++ show' kB) $ do
    ys1 <- sampleKernel (kernel (alphas ! kB) (ls ! kB)) xs
    ys2 <- sampleKernel (kernel (alphas ! kB) (ls ! kB)) xs
    display $ do
      localViewSet titleL "A" $
        plotLines xs ys1
      localViewSet titleL "B" $
        plotLines xs ys2
      forLM pis $ \ theta ->
        let ys3 = cos theta V.*% ys1 V.%+% sin theta V.*% ys2
            title = printf "a=%f l=%f theta=%f" (alphas ! kB) (ls ! kB) theta
        in
        localViewSet titleL title $
          plotLines xs ys3

trainedKernelParams :: Kernel -> V i Double -> V i Double -> V j Double -> (V j Double, M j j Double)
trainedKernelParams k trainingXs trainingYs predictionXs = (mean, cov)
  where
    kPredictionPrediction = buildKernel k predictionXs predictionXs
    kTrainingTraining     = buildKernel k trainingXs   trainingXs
    kPredictionTraining   = buildKernel k predictionXs trainingXs
    kTrainingTrainingInv  = M.inv kTrainingTraining
    mean = M.colToVector $
      kPredictionTraining
      M.%*% kTrainingTrainingInv
      M.%*% M.colFromVector trainingYs
    cov =
      kPredictionPrediction
      M.%-% kPredictionTraining
      M.%*% kTrainingTrainingInv
      M.%*% M.trans kPredictionTraining

trainingXs :: V 4 Double
trainingXs = V.iterateN sint 1 (+1)

trainingYs :: V 4 Double
trainingYs = V.unsafeIV $ Vector.fromList [3, 6, 7, 2]

p13 :: IO ()
p13 = execMGenT $ execCPlotT defaultPlotEnv $ forLM hypers $ \ kB -> 
  let title = printf "a=%f l=%f" (alphas ! kB) (ls ! kB)
  in
  localViewSet pointSizeL 4 $
  localViewMod outFileL (++ show' kB) $
  display $ do
    plotPoints trainingXs trainingYs
    forLM (sint :: SInt 10) $ \ _ ->
      localViewSet titleL title $ do
        predictionYs <- uncurry sampleMVN $ trainedKernelParams (kernel (alphas ! kB) (ls ! kB)) trainingXs trainingYs xs
        plotLines xs predictionYs

p14 :: IO ()
p14 = execMGenT $ execCPlotT defaultPlotEnv $ forLM hypers $ \ kB ->
  let title = printf "a=%f l=%f" (alphas ! kB) (ls ! kB)
  in
  localViewSet pointSizeL 4 $
  localViewMod outFileL (++ show' kB) $
  display $ do
    let (tmean, tcov) = trainedKernelParams (kernel (alphas ! kB) (ls ! kB)) trainingXs trainingYs xs
        tvar = M.getDiag tcov
        tdev = cmap (\ x -> sqrt x * 2) tvar
    plotPoints trainingXs trainingYs
    localViewSet titleL title $
      plotLinesError xs tmean (tmean V.%-% tdev) (tmean V.%+% tdev)

main :: IO ()
main = return ()
