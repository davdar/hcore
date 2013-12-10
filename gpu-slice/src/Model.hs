module Model where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Primitive
import Data.Array.Accelerate (Acc)
import Data.Serialize
import Data.Vector.Cereal ()
import Statistics.Distribution hiding (mean)
import Statistics.Distribution.Normal
import Stats
import System.Random
import System.Random.MWC
import Types
import Util
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as A
import qualified Data.Array.Accelerate.IO as A
import qualified Data.Array.Repa as R
import qualified Data.ByteString as BS
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV

------------------------------------------------------------
--------------- Model Representations ----------------------
------------------------------------------------------------

data ModelVector = ModelVector
  { getMean1V :: VectorU Float
  , getVar1V :: Float
  , getMean2V :: VectorU Float
  , getVar2V :: Float
  , getMixV :: Float
  } deriving (Show)

data ModelRepa = ModelRepa
  { getMean1R :: RVectorU Float
  , getVar1R :: Float
  , getMean2R :: RVectorU Float
  , getVar2R :: Float
  , getMixR :: Float
  } deriving (Show)

type ModelAcc = 
  ( AVector Float
  , AScalar Float
  , AVector Float
  , AScalar Float
  , AScalar Float
  )

modelAccToVector :: ModelAcc -> ModelVector
modelAccToVector (mean1, var1, mean2, var2, mix) = ModelVector mean1' var1' mean2' var2' mix'
  where
    mean1' = accToVector mean1
    var1' = accToScalar var1
    mean2' = accToVector mean2
    var2' = accToScalar var2
    mix' = accToScalar mix

modelRepaToVector :: ModelRepa -> ModelVector
modelRepaToVector (ModelRepa mean1 var1 mean2 var2 mix) = (ModelVector mean1' var1 mean2' var2 mix)
  where
    mean1' = repaToVector mean1
    mean2' = repaToVector mean2

modelVectorToRepa :: ModelVector -> ModelRepa
modelVectorToRepa (ModelVector mean1 var1 mean2 var2 mix) = (ModelRepa mean1' var1 mean2' var2 mix)
  where
    mean1' = vectorToRepa mean1
    mean2' = vectorToRepa mean2

modelVectorToAcc :: ModelVector -> ModelAcc
modelVectorToAcc (ModelVector mean1 var1 mean2 var2 mix) = (mean1', var1', mean2', var2', mix')
  where
    mean1' = vectorToAcc mean1
    var1' = scalarToAcc var1
    mean2' = vectorToAcc mean2
    var2' = scalarToAcc var2
    mix' = scalarToAcc mix

modelRepaToAcc :: ModelRepa -> ModelAcc
modelRepaToAcc (ModelRepa mean1 var1 mean2 var2 mix) = (mean1', var1', mean2', var2', mix')
  where
    mean1' = repaToAcc mean1
    var1' = scalarToAcc var1
    mean2' = repaToAcc mean2
    var2' = scalarToAcc var2
    mix' = scalarToAcc mix

flattenModel :: ModelRepa -> VectorU Float
flattenModel (modelRepaToVector -> ModelVector mean1 var1 mean2 var2 mix) = UV.concat
  [ mean1
  , mean2
  , UV.fromList [var1, var2, mix]
  ]

unflattenModel :: VectorU Float -> ModelRepa
unflattenModel v = modelVectorToRepa $ ModelVector mean1 var1 mean2 var2 mix
  where
    mean1 = UV.slice 0 l v
    var1 = v UV.! (2 * l)
    mean2 = UV.slice l l v
    var2 = v UV.! (2 * l + 1)
    mix = v UV.! (2 * l + 2)
    l = (UV.length v - 3) `div` 2

------------------------------------------------------------
--------------- Model Likelihoods --------------------------
------------------------------------------------------------

lhdSerial :: RMatrixU Float -> ModelRepa -> Float
lhdSerial xs (ModelRepa mean1 var1 mean2 var2 mix) =
  let [n,m] = R.listOfShape $ R.extent xs
      lhd mean var i = R.foldAllS (*) 1 $ R.fromFunction (R.ix1 m) $ \ (R.listOfShape -> [j]) ->
        lhdNormal (mean R.! R.ix1 j) var (xs R.! R.ix2 i j)
  in 
  R.foldAllS (*) 1 $ R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
    lhd mean1 var1 i * mix + lhd mean2 var2 i * (1 - mix)

lhdPar :: RMatrixU Float -> ModelRepa -> Float
lhdPar xs (ModelRepa mean1 var1 mean2 var2 mix) =
  let [n,m] = R.listOfShape $ R.extent xs
      lhd mean var i = R.foldAllS (*) 1 $ R.fromFunction (R.ix1 m) $ \ (R.listOfShape -> [j]) ->
        lhdNormal (mean R.! R.ix1 j) var (xs R.! R.ix2 i j)
  in runIdentity $
  R.foldAllP (*) 1 $ R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
    lhd mean1 var1 i * mix + lhd mean2 var2 i * (1 - mix)

lhdAcc :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
lhdAcc xs (A.unlift -> (mean1 , var1 , mean2 , var2 , mix)) =
  let lhd mean var (A.unlift . A.unindex2 -> (i,j)) = 
        lhdNormalAcc (mean A.! A.index1 j) var (xs A.! A.index2 i j)
      m1 = A.map (* A.the mix) $ A.fold (*) 1 $ A.generate (A.shape xs) $ lhd mean1 $ A.the var1
      m2 = A.map (* (1 - A.the mix)) $ A.fold (*) 1 $ A.generate (A.shape xs) $ lhd mean2 $ A.the var2
  in A.product $ A.zipWith (+) m1 m2

------------------------------------------------------------
--------------- Full (joint) Model -------------------------
------------------------------------------------------------

makeJoint :: (RMatrixU Float -> ModelRepa -> Float) -> RMatrixU Float -> ModelRepa -> Float
makeJoint lhd x model@(ModelRepa mean1 var1 mean2 var2 mix) =  
  lhd x model
  * R.foldAllS (*) 1 (R.map (lhdNormal 0 1) mean1)
  * lhdGamma 1 1 var1
  * R.foldAllS (*) 1 (R.map (lhdNormal 0 1) mean2)
  * lhdGamma 1 1 var2
  * lhdNormal 0 1 (logit mix)

jointSerial :: RMatrixU Float -> ModelRepa -> Float
jointSerial = makeJoint lhdSerial

jointPar :: RMatrixU Float -> ModelRepa -> Float
jointPar = makeJoint lhdPar

jointAcc :: RMatrixU Float -> ModelRepa -> Float
jointAcc = makeJoint lhd
  where
    lhd xs = 
      let xsA = A.use $ A.fromRepa $ R.copyS xs
          step = A.run1 (lhdAcc xsA)
      in accToScalar . step . modelRepaToAcc

------------------------------------------------------------
--------------- Full (joint) Model -------------------------
------------------------------------------------------------

modelProposal :: Gen (PrimState IO) -> ModelRepa -> IO ModelRepa
modelProposal gen (ModelRepa mean1 var1 mean2 var2 mix) = do
  let stdev1 = sqrt var1
      stdev2 = sqrt var2
      lmix = logit mix
      model' = ModelRepa mean1 stdev1 mean2 stdev2 lmix
  (ModelRepa mean1' stdev1' mean2' stdev2' lmix') <- liftM unflattenModel $ mvGaussianProposal gen $ flattenModel model'
  return $ ModelRepa mean1' (stdev1' ^ (2::Int)) mean2' (stdev2' ^ (2::Int)) (logistic lmix')

------------------------------------------------------------
--------------- Concrete Models ----------------------------
------------------------------------------------------------

-- for MCMC initialization
model0 :: Int -> ModelVector
model0 n = ModelVector (UV.replicate n 0) 1 (UV.replicate n 0) 1 0.5

-- for data generation
model1 :: ModelVector
model1 = ModelVector (UV.fromList [-1,-1,-1]) 1 (UV.fromList [1,1,1]) 10 0.33

------------------------------------------------------------
--------------- Synthetic Data -----------------------------
------------------------------------------------------------

genData :: Int -> IO ()
genData = genDataModel model1

genDataModel :: ModelVector -> Int -> IO ()
genDataModel (ModelVector mean1 var1 mean2 var2 mix) n = withSystemRandom . asGenIO $ \ gen -> do
  v <- BV.replicateM n $ do
    u <- randomRIO (0::Float, 1)
    let (mean, var) = if u < mix
          then (mean1, var1)
          else (mean2, var2)
    UV.forM mean $ \ m ->
      liftM realToFrac $ genContVar (normalDistr (realToFrac m) $ realToFrac var) gen
  let a = convertFromBoxed v
      vv = R.toUnboxed a
  BS.writeFile "synthetic.data" $ encode (n, UV.length mean1, vv)
  where
    convertFromBoxed :: VectorB (VectorU Float) -> RMatrixU Float
    convertFromBoxed v = 
      R.computeS $ 
      R.fromFunction (R.ix2 n $ UV.length mean1) $ \ (R.listOfShape -> [j,i]) -> 
        (v BV.! i) UV.! j

readData :: IO (RArrayU R.DIM2 Float)
readData = do
  Right (i, j, v) <- liftM decode $ BS.readFile "data"
  return $ R.fromUnboxed (R.ix2 i j) v

dataSize :: IO R.DIM2
dataSize = do
  d <- readData
  return $ R.extent d

------------------------------------------------------------
--------------- Inference ----------------------------------
------------------------------------------------------------

modelMAP :: (Monad m) => VectorB ModelRepa -> m ModelRepa
modelMAP ms = do
  let n = BV.length ms
      nf = fromIntegral n
      [m] = R.listOfShape $ R.extent $ getMean1R $ ms BV.! 0
      mean1s = R.fromFunction (R.ix2 n m) $ \ (R.listOfShape -> [i, j]) ->
        getMean1R (ms BV.! i) R.! R.ix1 j
      var1s = R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
        getVar1R (ms BV.! i)
      mean2s = R.fromFunction (R.ix2 n m) $ \ (R.listOfShape -> [i, j]) ->
        getMean2R (ms BV.! i) R.! R.ix1 j
      var2s = R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
        getVar2R (ms BV.! i)
      mixs = R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
        getMixR (ms BV.! i)
  mean1 <- R.foldP (+) 0 $ R.transpose $ R.map (/ nf) mean1s
  mean2 <- R.foldP (+) 0 $ R.transpose $ R.map (/ nf) mean2s
  var1 <- R.foldAllP (+) 0 $ R.map (/ nf) var1s
  var2 <- R.foldAllP (+) 0 $ R.map (/ nf) var2s
  mix <- R.foldAllP (+) 0 $ R.map (/ nf) mixs
  return $ ModelRepa mean1 var1 mean2 var2 mix

numIters :: Int
numIters = 100

mainWith :: (RMatrixU Float -> ModelRepa -> Float) -> IO ()
mainWith jointPdf = withSystemRandom . asGenIO $ \ gen -> do
  xs <- readData
  let [_,m] = R.listOfShape $ R.extent xs
  (samples, successes) <- mh (modelProposal gen) (jointPdf xs) (modelVectorToRepa $ model0 m) numIters
  print "SUCCESSES"
  print successes
  print "MAP"
  print =<< modelMAP samples

mainSerial :: IO ()
mainSerial = mainWith jointSerial

mainPar :: IO ()
mainPar = mainWith jointPar

mainAcc :: IO ()
mainAcc = mainWith jointAcc

