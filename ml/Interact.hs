{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception.Base
import Data.Int.Indexed
import Data.M.Storable (M)
import Data.V.Storable (V, Storable)
import FP
import Statistics.Distribution.Uniform
import ML.Gen
import ML.MonadGen
import ML.Statistics
import Prelude ()
import Statistics.Distribution.Normal
import System.IO.Unsafe
import Vis.Plot
import qualified Data.L as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.M.Generic as M hiding (trans)
import qualified Data.M.Storable as M
import qualified Data.V.Generic as V hiding (fromL, iterateN, fill, fillM, singleton, build, empty)
import qualified Data.V.Storable as V
import qualified Data.V.Boxed as VB
import qualified Data.Vector.Storable as Vector

type Kernel = Double -> Double -> Double

dim :: SInt 100
dim = sint

xs :: V 100 Double
xs = V.grid sint 0 5

pis :: V 10 Double
pis = V.grid sint (negate pi) pi

alphas :: V 4 Double
alphas = V.fromL $ L.unsafeL [10, 20, 200, 1000]

ls :: V 10 Double
ls = V.iterateN sint 0.1 (+ 0.2)

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
  snSamples <- V.fillM (V.length mean) $ 
    genContVarM $ normalDistr 0 1
  return $ M.colToVector $ 
    M.colFromVector mean 
    M.%+% M.trans (M.cholesky covariance)
    M.%*% M.colFromVector snSamples

buildKernel :: Kernel -> V i Double -> V j Double -> M i j Double
buildKernel k xs xs' = M.build (V.length xs) (V.length xs') $ \ (iS, jS) -> 
  k (xs ! iS) (xs' ! jS)

sampleKernel :: (MonadGen m) => Kernel -> V i Double -> m (V i Double)
sampleKernel k xs = sampleMVN (V.fill (V.length xs) 0) $ buildKernel k xs xs

p11 :: IO ()
p11 = execMGenT $ execCPlotT defaultPlotEnv $ 
  forLM sint $ \ akB ->
  forLM sint $ \ lkB ->
  let title = printf "a=%f l=%f" (alphas ! akB) (ls ! lkB)
  in 
  localViewMod outFileL (++ show' akB ++ show' lkB) $
  localViewSet titleL title $
  display $
    forLM (sint :: SInt 10) $ \ _ -> do
      ys <- sampleKernel (kernel (alphas ! akB) (ls ! lkB)) xs
      plotLines (V.fromV xs) (V.fromV ys)

p12 :: IO ()
p12 = execMGenT $ execCPlotT defaultPlotEnv $ 
  forLM sint $ \ akB ->
  forLM sint $ \ lkB ->
  localViewMod outFileL (++ show' akB ++ show' lkB) $ do
    ys1 <- sampleKernel (kernel (alphas ! akB) (ls ! lkB)) xs
    ys2 <- sampleKernel (kernel (alphas ! akB) (ls ! lkB)) xs
    display $ do
      localViewSet titleL "A" $
        plotLines (V.fromV xs) (V.fromV ys1)
      localViewSet titleL "B" $
        plotLines (V.fromV xs) (V.fromV ys2)
      forLM pis $ \ theta ->
        let ys3 = cos theta V.*% ys1 V.%+% sin theta V.*% ys2
            title = printf "a=%f l=%f theta=%f" (alphas ! akB) (ls ! lkB) theta
        in
        localViewSet titleL title $
          plotLines (V.fromV xs) (V.fromV ys3)

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
p13 = execMGenT $ execCPlotT defaultPlotEnv $ 
  forLM sint $ \ akB -> 
  forLM sint $ \ lkB ->
  let title = printf "a=%f l=%f" (alphas ! akB) (ls ! lkB)
  in
  localViewSet pointSizeL 4 $
  localViewMod outFileL (++ show' akB ++ show' lkB) $
  display $ do
    plotPoints (V.fromV trainingXs) (V.fromV trainingYs)
    forLM (sint :: SInt 10) $ \ _ ->
      let k = kernel (alphas ! akB) (ls ! lkB)
      in
      localViewSet titleL title $ do
        predictionYs <- uncurry sampleMVN $ trainedKernelParams k trainingXs trainingYs xs
        plotLines (V.fromV xs) (V.fromV predictionYs)

p14 :: IO ()
p14 = execMGenT $ execCPlotT defaultPlotEnv $ 
  forLM sint $ \ akB ->
  forLM sint $ \ lkB ->
  let title = printf "a=%f l=%f" (alphas ! akB) (ls ! lkB)
  in
  localViewSet pointSizeL 4 $
  localViewMod outFileL (++ show' akB ++ show' lkB) $
  display $ do
    let k = kernel (alphas ! akB) (ls ! lkB)
        (tmean, tcov) = trainedKernelParams k trainingXs trainingYs xs
        tvar = M.getDiag tcov
        tdev = cmap (\ x -> sqrt x * 2) tvar
    localViewSet titleL title $
      plotLinesError (V.fromV xs) (V.fromV tmean) (V.fromV $ tmean V.%-% tdev) (V.fromV $ tmean V.%+% tdev)
    plotPoints (V.fromV trainingXs) (V.fromV trainingYs)

logLikelihoodMVN :: V i Double -> M i i Double -> V i Double -> Double
logLikelihoodMVN mean covariance ys =
  (-1 / 2) 
  * log 
    ( 2 
    * pi ^ (fromIntegral $ stripI $ V.length mean) 
    * M.det (M.trans $ M.cholesky covariance)
    )
  + 
  (-1 / 2)
  * M.fromSingleton
    (      M.rowFromVector ys 
     M.%*% M.inv covariance 
     M.%*% M.colFromVector ys
    )

p15 :: IO ()
p15 = execMGenT $ execCPlotT defaultPlotEnv $
  let vs = ($ []) $ appEndo $ execWriter $
        forLM sint $ \ akB ->
        forLM sint $ \ lkB ->
          let title = printf "\"a=%.2g  l=%.2g\"" (alphas ! akB) (ls ! lkB)
              k = kernel (alphas ! akB) (ls ! lkB)
              (tmean, tcov) = trainedKernelParams k trainingXs trainingYs xs
              llhd = logLikelihoodMVN (V.fill sint 0) (buildKernel k trainingXs trainingXs) trainingYs
          in
          tell $ Endo $ (:) (title, llhd)
  in
  L.exL vs $ \ vsS ->
  let vsV = VB.fromL vsS
      tsV = cmap (BS.pack . fst) vsV
      psV = cmap snd vsV
  in
  display $
    labeledHist tsV psV

sampleCategorical :: forall m i. (MonadGen m) => V i Double -> m (BInt i)
sampleCategorical v =
  assert (V.sum v `deqv` 1.0) $ do
    let v' = V.build (V.length v) $ \ iB ->
          bintElim iB $ \ jS jLti ->
          iterDoL (jS |+| (sint :: SInt 1)) 0 $ \ jB ->
            (+) $ v ! bintExtend jB (lteSuccFromLt jLti)
    s <- genContVarM $ uniformDistr 0 1
    return $ fromJust $ iiterDoL v' Nothing $ \ iB n ->
      flip mplus $ do
        guard (s < n)
        return iB

chineseRestaurantProc :: (MonadGen m) => Double -> Int -> V i Int -> m (ExI V Int)
chineseRestaurantProc alpha n tables = do
  let tablesP = 
        cmap ((/ (alpha + fromIntegral n)) . fromIntegral) tables 
        `V.concat` V.singleton (alpha / (alpha + fromIntegral n))
  c <- sampleCategorical tablesP
  return $
    bintElim c $ \ cS cLtip1 -> 
    case cS `icompare` V.length tables of
      ILt pf ->
        let c' = bint cS pf
        in ExI $ V.updateIndex c' ((tables ! c') + 1) tables
      IEq -> ExI $ tables `V.concat` V.singleton 1
      IGt pf -> ltAbsurdLte pf $ lteFromLtSucc cLtip1

chineseRestaurant :: forall m. (MonadGen m) => Double -> Int -> m (ExI V Int)
chineseRestaurant alpha' n = loop alpha' 0 $ ExI V.empty
  where
    loop :: Double -> Int -> ExI V Int -> m (ExI V Int)
    loop alpha m vE 
      | m == n = return vE
      | otherwise =
          unExI vE $ \ v ->
          loop alpha (m+1) =<< chineseRestaurantProc alpha m v

data StateSpace i j k = StateSpace
  { getDatas      :: M i j Double
  , getAlpha      :: Double
  , getLambda     :: Double
  , getR          :: Double
  , getMu         :: M k j Double
  , getBeta       :: Double
  , getInverseW   :: Double
  , getInverseS   :: V k Double
  , getComponents :: V i (BInt k)
  , getTables     :: V k Int
  }

data ExStateSpace i j where
  ExStateSpace :: forall i j k. StateSpace i j k -> ExStateSpace i j

sampleLambda :: (MonadGen m) => StateSpace i j k -> m (ExStateSpace i j)
sampleLambda = undefined

sampleR :: (MonadGen m) => StateSpace i j k -> m (ExStateSpace i j)
sampleR = undefined

sampleMuK :: (MonadGen m) => BInt k -> StateSpace i j k -> m (ExStateSpace i j)
sampleMuK = undefined

sampleBeta :: (MonadGen m) => Double -> StateSpace i j k -> m (ExStateSpace i j)
sampleBeta = undefined

sampleInverseW :: (MonadGen m) => StateSpace i j k -> m (ExStateSpace i j)
sampleInverseW = undefined

sampleInverseSK :: (MonadGen m) => BInt k -> StateSpace i j k -> m (ExStateSpace i j)
sampleInverseSK = undefined

sampleComponentI :: forall i k m. (MonadGen m) => BInt i -> StateSpace i j k -> m (ExStateSpace i j)
sampleComponentI iB ss@(StateSpace datas alpha lambda r mu beta inverseW inverseS components tables) = do
  let numPeople           = fromIntegral $ stripI $ V.length components
      numPeopleAtTable kB = fromIntegral $ stripI $ tablesWithoutMe ! kB
      myTable             = components ! iB
      tablesWithoutMe     = V.modifyIndex myTable (- 1) tables

      priorTable kB = numPeopleAtTable kB / (numPeople - 1 + alpha)
      prioNewTable = alpha / (numPeople - 1 + alpha)
      priorAllTables :: V (k+1) Double
      priorAllTables = V.build sint priorTable `V.concat` V.singleton priorNewTable

      likelihoodTable kB = likelihoodMVN (mu `M.getRow` kB) (M.diag sint sint (1 / (inverseS ! kB))) (datas `M.getCol` iB)
      likelihoodNewTable = 1
      likelihoodAllTables :: V (k+1) Double
      likelihoodAllTables = V.build sint likelihoodTable `V.concat` V.singleton likelihoodNewTable

      posteriorAllTables :: V (k+1) Double
      posteriorAllTables = V.build sint $ \ kB -> (priorAllTables ! kB) * (likelihoodAllTables ! kB)

      normalize v = cmap (/ V.sum v) v

  (c :: BInt (k+1)) <- sampleCategorical $ normalize posteriorAllTables
  bintElim c $ \ (cS :: SInt k') (cLtip1 :: k' < (k + 1)) -> 
    case cS `icompare` V.length tables of
      ILt (pf :: k' < k) ->
        let c' = bint cS pf
        in return $ ExStateSpace $ gc $ ss
          { getComponents = V.updateIndex iB c' components
          , getTables     = V.modifyIndex c' (+1) tablesWithoutMe
          }
      IEq -> do
        newMu <- ...
        newInverseS <- ...
        let c' = bint cS undefined
        return $ ExStateSpace $ gc $ ss
          { getComponents = V.updateIndex iB c'
          , getTables     = tablesWithoutMe `V.concat` V.singleton 1
          , getMu         = newMu
          , getInverseS   = newInverseS
          }
      IGt (pf :: k' > k) -> ltAbsurdLte pf $ lteFromLtSucc cLtip1

main :: IO ()
main = return ()
