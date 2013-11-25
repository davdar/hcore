{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module ML.Bullshit where

import Unsafe.Coerce
import Text.CSV
import Control.Exception.Base
import Data.Int.Indexed
import Data.M.Storable (M)
import Data.V.Storable (V, Storable)
import FP
import Statistics.Distribution.Uniform
import Statistics.Distribution.Gamma
import ML.Gen
import ML.MonadGen
import ML.Statistics
import Prelude ()
import Statistics.Distribution.Normal
import System.IO.Unsafe
import Vis.Plot
import qualified Data.List as List
import qualified Data.L as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.M.Generic as M hiding (trans)
import qualified Data.M.Storable as M
import qualified Data.V.Generic as V hiding (fromL, iterateN, fill, fillM, singleton, build, empty, buildM)
import qualified Data.V.Generic as VG
import qualified Data.V.Storable as V
import qualified Data.V.Boxed as VB
import qualified Data.Vector.Storable as Vector

neg = negate

type Kernel = Double -> Double -> Double

dim :: SInt 100
dim = sint

xs :: V 100 Double
xs = V.grid sint 0 5

pis :: V 10 Double
pis = V.grid sint (negate pi) pi

--alphas :: V 4 Double
alphas :: V 3 Double
--alphas = V.fromL $ L.unsafeL [10, 20, 200, 1000]
alphas = V.fromL $ L.unsafeL [10, 100, 1000]

--ls :: V 10 Double
ls :: V 3 Double
--ls = V.iterateN sint 0.1 (+ 0.2)
ls = V.iterateN sint 0.1 (+ 0.6)

kernel :: Double -> Double -> Double -> Double -> Double
kernel alpha l x x' = 
  alpha 
  * exp (neg 1 / (2 * l ^ 2) * (x - x') ^ 2)
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
  (neg 1 / 2) 
  * log 
    ( 2 
    * pi ^ (fromIntegral $ stripI $ V.length mean) 
    * M.det (M.trans $ M.cholesky covariance)
    )
  + 
  (neg 1 / 2)
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

plotChineseRestaurant :: (MonadPlot m, MonadGen m) => Double -> Int -> m ()
plotChineseRestaurant alpha n = do
  tsE <- chineseRestaurant alpha n
  unExI tsE $ \ ts ->
    localViewMod outFileL (++ show alpha) $
    display $
    labeledHist (VB.build (V.length ts) (\ nB -> BS.pack $ show nB)) (V.fromV $ cmap fromIntegral ts)


likelihoodMVN :: V i Double -> M i i Double -> V i Double -> Double
likelihoodMVN mean covariance x =
  let n = fromIntegral $ stripI $ V.length mean
  in
  (2 * pi) ** (neg 1 * n / 2)
  * (M.det covariance) ** (neg 1 / 2)
  * exp
    ( neg 1
    / 2
    * M.fromSingleton
      (     M.rowFromVector (x V.%-% mean)
      M.%*% M.inv (covariance)
      M.%*% M.colFromVector (x V.%-% mean)
      )
    )

data DataSummary i j = DataSummary
  { getDatas :: M i j Double
  , meanDatas :: V j Double
  , covarianceDatas :: M j j Double
  }

summarize :: M i j Double -> DataSummary i j
summarize d =
  let mean = calcMean d
  in DataSummary d mean $ calcCovariance mean d

calcMean :: M i j Double -> V j Double
calcMean m = V.build (M.cols m) $ \ jB ->
  V.sum (M.getCol jB m) / (fromIntegral $ stripI $ M.rows m)

calcCovariance :: forall i j. V j Double -> M i j Double -> M j j Double
calcCovariance mean m =
  let n = fromIntegral $ stripI $ M.rows m
  in 
  cmap (* (1 / (n - 1))) $
  iterL (M.%+%) (M.fill (M.cols m) (M.cols m) 0) $
  VB.build (M.rows m) $ \ (iB :: BInt i) ->
    let diff :: V j Double
        diff = M.getRow iB m V.%-% mean
    in M.colFromVector diff M.%*% M.rowFromVector diff

{-
sampleWishart :: (MonadGen m) => Int -> M i i Double -> m (M i i Double)
sampleWishart n sigma =
  unEx (sintEx n) $ \ nS ->
  iterDoLM nS (M.fill (M.rows sigma) (M.cols sigma) 0) $ \ _ t -> do
    x <- sampleMVN (V.fill (M.rows sigma) 0) sigma
    return $ t M.%+% M.colFromVector x M.%*% M.rowFromVector x
    -}

sampleWishart :: (MonadGen m) => Double -> M i i Double -> m (M i i Double)
sampleWishart k sigma = do
  b <- M.buildM (M.rows sigma) (M.cols sigma) $ \ (iB,jB) ->
    let i = stripI iB
        j = stripI jB
    in
    case i `compare` j of
      LT -> return 0
      EQ -> sample (gammaDistr (k - fromIntegral i + 1) 2)
      GT -> sample (normalDistr 0 1)
  return $
    let a = M.cholesky sigma
        ab = a M.%*% b
    in ab M.%*% M.trans ab

sampleLambda :: (MonadGen m) => DataSummary i j -> m (V j Double)
sampleLambda s = sampleMVN (meanDatas s) $ covarianceDatas s

sampleR :: (MonadGen m) => DataSummary i j -> m (M j j Double)
sampleR s = sampleWishart 1 $ M.inv $ covarianceDatas s

sampleMuK :: (MonadGen m) => BInt k -> DataSummary i j -> V i (BInt k) -> VB.V k (M j j Double) -> V k Int -> V j Double -> M j j Double -> m (V j Double)
sampleMuK kB datas components inverseS tables lambda r = sampleMVN (M.rowToVector mean) covariance
  where
    meanForClass = iterDoL (M.rows $ getDatas datas) (V.fill (M.cols $ getDatas datas) 0) $ \ iB ->
      if components ! iB == kB
        then (V.%+%) $ M.getRow iB $ getDatas datas 
        else id
    meanNumerator = M.rowFromVector meanForClass M.%*% (inverseS ! kB) M.%+% M.rowFromVector lambda M.%*% r
    meanDenominator = cmap (* fromIntegral (tables ! kB)) (inverseS ! kB) M.%+% r
    mean = meanNumerator M.%*% M.inv meanDenominator
    covariance = M.inv meanDenominator
      
sampleBeta :: (MonadGen m) => Int -> m Double
sampleBeta dim = do
  s <- sample (gammaDistr 1 $ fromIntegral dim)
  return $ (1 / s) + fromIntegral dim - 1

sampleInverseW :: (MonadGen m) => DataSummary i j -> m (M j j Double)
sampleInverseW s = liftM M.inv $ sampleWishart 1 $ covarianceDatas s

sampleInverseSK :: (MonadGen m) => BInt k -> DataSummary i j -> Double -> M k j Double -> M j j Double -> V k Int -> V i (BInt k) -> m (M j j Double)
sampleInverseSK kB datas beta mu inverseW tables components = sampleWishart thing1 thing2
  where
    thing1 = beta + fromIntegral (tables ! kB)
    thing2 = cmap (thing1 *) $ M.inv (cmap (* beta) (M.inv inverseW) M.%+% meanThing)
    meanThing = iterDoL (M.rows $ getDatas datas) (M.fill (M.cols $ getDatas datas) (M.cols $ getDatas datas) 0) $ \ iB ->
      if components ! iB == kB
        then (M.%+%) $ 
          let bob = M.getRow iB (getDatas datas) V.%-% M.getRow kB mu
          in M.colFromVector bob M.%*% M.rowFromVector bob
        else id

sampleInverseSKDegenerate :: (MonadGen m) => Double -> M j j Double -> m (M j j Double)
sampleInverseSKDegenerate beta inverseW = sampleWishart thing1 thing2
  where
    thing1 = beta + 1
    thing2 = cmap ((thing1 / beta) *) inverseW

data TableInfo (i::Nat) (k::Nat) = TableInfo
  { personToTableAssignments :: V i (BInt k)
  , tablePopulations :: V k Int
  }

type MaybeIncr t k = Either (t k) (t (k+1))

sampleTableInfoI :: forall i j k m. (MonadGen m) 
  => Double                 -- alpha
  -> Double                 -- beta
  -> DataSummary i j        -- datas
  -> M k j Double           -- mu
  -> M j j Double           -- inverseW
  -> VB.V k (M j j Double)  -- inverseS
  -> V i (BInt k)           -- components
  -> V k Int                -- tables
  -> BInt i                 -- iB
  -> m (MaybeIncr (TableInfo i) k)
sampleTableInfoI alpha beta datas mu inverseW inverseS components tables iB = do
  let numPeople           = fromIntegral $ stripI $ M.rows $ getDatas datas
      numPeopleAtTable kB = fromIntegral $ tablesWithoutMe ! kB
      myTable             :: BInt k
      myTable             = components ! iB
      tablesWithoutMe     :: V k Int
      tablesWithoutMe     = V.modifyIndex myTable (\ x -> x - 1) tables

      priorTable kB       = numPeopleAtTable kB / (numPeople - 1 + alpha)
      priorNewTable       = alpha / (numPeople - 1 + alpha)
      priorAllTables      :: V (k+1) Double
      priorAllTables      = V.build (V.length tables) priorTable `V.concat` V.singleton priorNewTable

      likelihoodTable     :: BInt k -> Double
      likelihoodTable kB  = likelihoodMVN (M.getRow kB mu) (M.inv (inverseS ! kB)) (M.getRow iB $ getDatas datas)

  imaginedNewInverseS <- sampleInverseSKDegenerate beta inverseW
  let imaginedNewMu       = M.getRow iB $ getDatas datas
      likelihoodNewTable  = likelihoodMVN imaginedNewMu imaginedNewInverseS (M.getRow iB $ getDatas datas)
      likelihoodAllTables :: V (k+1) Double
      likelihoodAllTables = V.build (V.length tables) likelihoodTable `V.concat` V.singleton likelihoodNewTable

      posteriorAllTables  :: V (k+1) Double
      posteriorAllTables  = V.build (V.length priorAllTables) $ \ kB -> (priorAllTables ! kB) * (likelihoodAllTables ! kB)

      normalize v = cmap (/ V.sum v) v

  (myNewTable :: BInt (k+1)) <- sampleCategorical $ normalize posteriorAllTables
  bintElim myNewTable $ \ (myNewTableS :: SInt k') (k'ltkp1 :: k' < (k + 1)) -> 
    return $
    case myNewTableS `icompare` V.length tables of
      ILt (pf :: k' < k) ->
        let myNewTableB = bint myNewTableS pf
        in Left $ TableInfo
          { personToTableAssignments = V.updateIndex iB myNewTableB components
          , tablePopulations         = V.modifyIndex myNewTableB (+ 1) tablesWithoutMe
          }
      IEq ->
        let myNewTableB = bint myNewTableS k'ltkp1
        in Right $ TableInfo
          { personToTableAssignments = V.updateIndex iB myNewTableB $ cmap (flip bintExtend lteSucc) components
          , tablePopulations         = tablesWithoutMe `V.concat` V.singleton 1
          }
      IGt (pf :: k < k') -> ltAbsurdLte pf $ lteFromLtSucc k'ltkp1

data StateSpace (i::Nat) (j::Nat) (k::Nat) = StateSpace
  { ssLambda     :: V j Double
  , ssR          :: M j j Double
  , ssMu         :: M k j Double
  , ssBeta       :: Double
  , ssInverseW   :: M j j Double
  , ssInverseS   :: VB.V k (M j j Double)
  , ssComponents :: V i (BInt k)
  , ssTables     :: V k Int
  }

data StateSpaceEx i j where
  StateSpaceEx :: forall i j k. StateSpace i j k -> StateSpaceEx i j

data UnsafeDecr (i::Nat) where
  UnsafeDecr :: forall i. SInt i -> UnsafeDecr (i+1)

unsafeDecr :: SInt i -> UnsafeDecr i
unsafeDecr x = unsafeCoerce $ UnsafeDecr x

sampleStep :: forall m i j k. (MonadGen m) => Double -> DataSummary i j -> StateSpace i j k -> m (StateSpaceEx i j)
sampleStep alpha datas ss0 = do
  lambda' <- sampleLambda datas
  let ss1 = ss0 { ssLambda = lambda' }

  r' <- sampleR datas
  let ss2 = ss1 { ssR = r' }

  mu' <- liftM (M.fromNested (V.length $ ssTables ss2) (V.length $ ssLambda ss2)) $ 
    VB.buildM (V.length $ ssTables ss2) $ \ kB ->
      sampleMuK kB datas (ssComponents ss2) (ssInverseS ss2) (ssTables ss2) (ssLambda ss2) (ssR ss2) :: m (V j Double)
  let ss3 = ss2 { ssMu = mu' }

  beta' <- sampleBeta $ stripI $ V.length $ ssLambda ss3
  let ss4 = ss3 { ssBeta = beta' }

  inverseW' <- sampleInverseW datas
  let ss5 = ss4 { ssInverseW = inverseW' }

  inverseS' <- VB.buildM (V.length $ ssTables ss5) $ \ kB ->
    sampleInverseSK kB datas (ssBeta ss5) (ssMu ss5) (ssInverseW ss5) (ssTables ss5) (ssComponents ss5)
  let ss6 = ss5 { ssInverseS = inverseS'}

  ss7E <- iterDoLM (V.length $ ssComponents ss6) (StateSpaceEx ss6) $ \ iB ssE' ->
    case ssE' of
      StateSpaceEx (ss' :: StateSpace i j k') -> do
        tinfo <- sampleTableInfoI alpha (ssBeta ss') datas (ssMu ss') (ssInverseW ss') (ssInverseS ss') (ssComponents ss') (ssTables ss') iB
        -- () <- trace' "k=" (V.length $ ssTables ss') $ return ()
        () <- return $ unsafePerformIO $ putStr "."
        case tinfo of
          Left (TableInfo components' (tables' :: V k' Int)) -> return $ StateSpaceEx $ ss'
            { ssComponents = components'
            , ssTables = tables'
            }
          Right (TableInfo components' (tables' :: V (k'+1) Int)) -> do
            let kB :: BInt (k'+1)
                kB = unsafeI $ stripI (V.length tables') - 1
            let newMu = M.getRow iB $ getDatas datas
                mu' = ssMu ss' `M.rowConcat` M.rowFromVector newMu
            newInverseS <- sampleInverseSK kB datas (ssBeta ss') mu' (ssInverseW ss') tables' components'
            let inverses' :: VB.V (k'+1) (M j j Double)
                inverses' = ssInverseS ss' `V.concat` VB.singleton newInverseS
            return $ StateSpaceEx $ ss'
              { ssComponents = components'
              , ssTables = tables'
              , ssMu = mu'
              , ssInverseS = inverses'
              }
  () <- return $ unsafePerformIO $ putStr "\n"
  return $ case ss7E of
    StateSpaceEx ss7 -> gc $ ss7
  where
    gc :: forall k. StateSpace i j k -> StateSpaceEx i j
    gc ss = case getEmptyTable $ ssTables ss of
      Nothing -> StateSpaceEx ss
      Just (kB :: BInt k) ->
        let kBad :: BInt (0+1)
            kBad = unsafeCoerce kB
            ssBad :: StateSpace i j (0+1)
            ssBad = unsafeCoerce ss
            ss' :: StateSpace i j 0
            ss' = ssBad
              { ssTables = removeIndex kBad $ ssTables ssBad
              , ssComponents = flip cmap (ssComponents ssBad) $ \ kB' -> 
                  if stripI kB' > stripI kB 
                    then unsafeI $ stripI kB' - 1
                    else unsafeI $ stripI kB'
              , ssMu = removeRowIndex kBad $ ssMu ssBad
              , ssInverseS = removeIndex kBad $ ssInverseS ssBad
              }
        in
        gc ss'
    getEmptyTable :: forall k. V k Int -> Maybe (BInt k)
    getEmptyTable tables = iiterDoL tables Nothing $ \ kB n ->
      flip mplus $ do
        guard (n == 0)
        return kB
    removeIndex :: forall t i a. (V.IVector t a, V.IVectorComplete t (i+1) a, V.IVectorComplete t i a, Elem (t (i+1) a) ~ a)
       => BInt (i+1) -> t (i+1) a -> t i a
    removeIndex iB v = VG.build (unsafeI $ stripI (V.length v) - 1) $ \ iB' ->
      if stripI iB' < stripI iB 
        then v ! (unsafeI $ stripI iB')
        else v ! (unsafeI $ stripI iB' + 1)
    removeRowIndex :: forall i j. BInt (i+1) -> M (i+1) j Double -> M i j Double
    removeRowIndex iB m = M.build (unsafeI $ stripI (M.rows m) - 1) (M.cols m) $ \ (iB' :: BInt i, jB :: BInt j) ->
      if stripI iB' < stripI iB
        then m ! (unsafeI $ stripI iB'     :: BInt (i+1), jB)
        else m ! (unsafeI $ stripI iB' + 1 :: BInt (i+1), jB)

sampleLoop :: (MonadGen m) => Int -> Double -> DataSummary i j -> StateSpace i j k -> m (StateSpaceEx i j)
sampleLoop n alpha datas ss 
  | n <= 0 = return $ StateSpaceEx ss
  | otherwise = do
      () <- trace' "step=" n $ return ()
      ss'E <- sampleStep alpha datas ss
      case ss'E of
        StateSpaceEx ss' -> sampleLoop (n-1) alpha datas ss'

data ExM a where
  ExMatrix :: forall i j a. M i j a -> ExM a

readData :: IO (ExM Double)
readData = do
  Right (stringData :: [[String]]) <- parseCSVFromFile "noisy-200.csv"
  return $ 
    let numRows :: Int
        numRows = List.length weeded
        numCols :: Int
        numCols = List.length (weeded List.!! 0)
        weeded = weed stringData
        weed :: [[String]] -> [[String]]
        weed = List.delete [""]
    in
    unEx (sintEx numRows) $ \ (iS :: SInt i) ->
    unEx (sintEx numCols) $ \ (jS :: SInt j) -> 
    let convertRow :: [String] -> V j Double
        convertRow ss = V.fromL $ L.unsafeL $ map read ss
        convertRows :: [V j Double] -> VB.V i (V j Double)
        convertRows = VB.fromL . L.unsafeL
    in 
    ExMatrix $ M.fromNested iS jS $ convertRows $ map convertRow $ weeded

main' :: IO ()
main' = do
  dE <- readData
  case dE of
    ExMatrix (datas :: M i j Double) -> do
      let ss0 = StateSpace
            { ssLambda = V.fill (M.cols datas) 1
            , ssR = M.fill (M.cols datas) (M.cols datas) 1
            , ssMu = M.fill (sint :: SInt 1) (M.cols datas) 1
            , ssBeta = 1
            , ssInverseW = M.fill (M.cols datas) (M.cols datas) 1
            , ssInverseS = VB.singleton (M.fill (M.cols datas) (M.cols datas) 1)
            , ssComponents = V.fill (M.rows datas) (unsafeI 0)
            , ssTables = V.singleton (stripI $ M.rows datas)
            }
          steps = 5000
          alpha = 0.000005
      ss'E <- execMGen $ sampleLoop steps alpha (summarize datas) ss0
      case ss'E of
        StateSpaceEx (ss' :: StateSpace i j k) ->
          withEqRefl (unsafeEqRefl :: j :=: 2) $
          plotit $ do
            plotPoints (V.fromV $ M.getCol (unsafeI 0) datas) (V.fromV $ M.getCol (unsafeI 1) datas)
            plotPoints (V.fromV $ M.getCol (unsafeI 0) $ ssMu ss') (V.fromV $ M.getCol (unsafeI 1) $ ssMu ss')
            forLM (V.length (ssTables ss')) $ \ kB ->
              trace' (printf "cov-%s" (show kB)) (M.inv $ ssInverseS ss' ! kB) $ do
                plotCholEllipse (sint :: SInt 100) (M.getRow kB (ssMu ss')) (M.inv $ ssInverseS ss' ! kB) 1
                plotCholEllipse (sint :: SInt 100) (M.getRow kB (ssMu ss')) (M.inv $ ssInverseS ss' ! kB) 2

plotSpiral :: IO ()
plotSpiral = do
  dE <- readData
  case dE of
    ExMatrix (datas :: M i j Double) ->
      plotit $
      plotPoints (V.fromV $ M.getCol (unsafeI 0) datas) (V.fromV $ M.getCol (unsafeI 1) datas)

ellipse2D :: forall i. SInt i -> Double -> Double -> Double -> Double -> M i 2 Double
ellipse2D n x y a b = 
  let ts = V.grid n 0 (2 * pi) :: V i Double
  in M.fromNested n sint $ VB.build n $ \ iB ->
    let t = ts ! iB
    in V.fromL $ L.unsafeL [x + a * cos t, y + b * sin t] :: V 2 Double

plotEllipse2D :: forall i. SInt i -> Double -> Double -> Double -> Double -> IO ()
plotEllipse2D n x y a b =
  let pts = ellipse2D n x y a b
  in plotit $ plotLines (V.fromV $ M.getCol (unsafeI 0) pts) (V.fromV $ M.getCol (unsafeI 1) pts)

cholEllipse :: forall i. SInt i -> V 2 Double -> M 2 2 Double -> Double -> M i 2 Double
cholEllipse n mean covariance scaling =
  let ch = M.cholesky covariance
      ts = V.grid n 0 (2 * pi) :: V i Double
  in M.fromNested n sint $ VB.build n $ \ iB ->
    let r = V.fromL $ L.unsafeL [cos (ts ! iB), sin (ts ! iB)] :: V 2 Double
    in M.colToVector $ cmap (* scaling) ch M.%*% M.colFromVector r M.%+% M.colFromVector mean

plotCholEllipse :: (MonadPlot m) => SInt i -> V 2 Double -> M 2 2 Double -> Double -> m ()
plotCholEllipse n mean covariance scaling =
  let pts = cholEllipse n mean covariance scaling
  in plotLines (V.fromV $ M.getCol (unsafeI 0) pts) (V.fromV $ M.getCol (unsafeI 1) pts)

