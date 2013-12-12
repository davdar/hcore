module Model where

import Control.DeepSeq.TH
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Primitive
import Data.Array.Accelerate (Acc, Exp)
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
$(deriveNFData ''ModelRepa)

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
  let [m,n] = R.listOfShape $ R.extent xs
      lhd mean var i = R.foldAllS (*) 1 $ R.fromFunction (R.ix1 m) $ \ (R.listOfShape -> [j]) ->
        lhdNormal (mean R.! R.ix1 j) var (xs R.! R.ix2 i j)
  in 
  R.foldAllS (*) 1 $ R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
    lhd mean1 var1 i * mix + lhd mean2 var2 i * (1 - mix)

llhdSerial :: RMatrixU Float -> ModelRepa -> Float
llhdSerial xs (ModelRepa mean1 var1 mean2 var2 mix) =
  let [m,n] = R.listOfShape $ R.extent xs
      lhd mean var i = R.foldAllS (*) 1 $ R.fromFunction (R.ix1 m) $ \ (R.listOfShape -> [j]) ->
        lhdNormal (mean R.! R.ix1 j) var (xs R.! R.ix2 i j)
  in 
  R.sumAllS $ R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
    log (lhd mean1 var1 i * mix + lhd mean2 var2 i * (1 - mix))

lhdPar :: RMatrixU Float -> ModelRepa -> Float
lhdPar xs (ModelRepa mean1 var1 mean2 var2 mix) =
  let [m,n] = R.listOfShape $ R.extent xs
      lhd mean var i = R.foldAllS (*) 1 $ R.fromFunction (R.ix1 m) $ \ (R.listOfShape -> [j]) ->
        lhdNormal (mean R.! R.ix1 j) var (xs R.! R.ix2 i j)
  in runIdentity $
  R.foldAllP (*) 1 $ R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
    lhd mean1 var1 i * mix + lhd mean2 var2 i * (1 - mix)

llhdPar :: RMatrixU Float -> ModelRepa -> Float
llhdPar xs (ModelRepa mean1 var1 mean2 var2 mix) =
  let [m,n] = R.listOfShape $ R.extent xs
      lhd mean var i = R.foldAllS (*) 1 $ R.fromFunction (R.ix1 m) $ \ (R.listOfShape -> [j]) ->
        lhdNormal (mean R.! R.ix1 j) var (xs R.! R.ix2 i j)
  in runIdentity $
  R.sumAllP $ R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
    log (lhd mean1 var1 i * mix + lhd mean2 var2 i * (1 - mix))

lhdAcc :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
lhdAcc xs (A.unlift -> (mean1 , var1 , mean2 , var2 , mix)) = 
  let lhd mean var (A.unlift . A.unindex2 -> (i,j)) = 
        lhdNormalAcc (mean A.! A.index1 j) var (xs A.! A.index2 i j)
      m1 = A.map (* A.the mix) $ A.fold (*) 1 $ A.generate (A.shape xs) $ lhd mean1 $ A.the var1
      m2 = A.map (* (1 - A.the mix)) $ A.fold (*) 1 $ A.generate (A.shape xs) $ lhd mean2 $ A.the var2
  in A.product $ A.zipWith (+) m1 m2

llhdAcc :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAcc xs (A.unlift -> (mean1, var1, mean2, var2, mix)) =
  let lhd mean var (A.unlift . A.unindex2 -> (i,j)) = 
        lhdNormalAcc (mean A.! A.index1 j) var (xs A.! A.index2 i j)
      m1 = A.map (* A.the mix) $ A.fold (*) 1 $ A.generate (A.shape xs) $ lhd mean1 $ A.the var1
      m2 = A.map (* (1 - A.the mix)) $ A.fold (*) 1 $ A.generate (A.shape xs) $ lhd mean2 $ A.the var2
  in A.sum $ A.zipWith (\ x1 x2 -> log $ x1 + x2) m1 m2

llhdAcc' :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAcc' xs (A.unlift -> (mean1, var1, mean2, var2, mix)) =
  let lhd mean var (A.unlift . A.unindex2 -> (i,j)) = 
        lhdNormalAcc (mean A.! A.index1 j) var (xs A.! A.index2 i j)
      m1 = A.map (* A.the mix) $ betterFold (*) 1 $ A.generate (A.shape xs) $ \ ij -> lhd mean1 (A.the var1) ij
      m2 = A.map (* (1 - A.the mix)) $ betterFold (*) 1 $ A.generate (A.shape xs) $ \ ij -> lhd mean2 (A.the var2) ij
  in A.sum $ A.zipWith (\ x1 x2 -> log $ x1 + x2) m1 m2
  where
    betterFold f x0 xs = 
      let (n::Exp Int,_::Exp Int) = A.unlift $ A.unindex2 $ A.shape xs
      in A.generate (A.index1 n) $ \ i -> A.sfoldl f x0 i xs

llhdAcc'' :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAcc'' xs (A.unlift -> (mean1, var1, mean2, var2, mix)) =
  let (n::Exp Int,_::Exp Int) = A.unlift $ A.unindex2 $ A.shape xs
      mean1R :: Acc (AMatrix Float)
      mean1R = A.replicate (A.lift (A.Z A.:. n A.:. A.All)) mean1
      mean2R :: Acc (AMatrix Float)
      mean2R = A.replicate (A.lift (A.Z A.:. n A.:. A.All)) mean2
      m1 :: Acc (AMatrix Float)
      m1 = A.zipWith (combine $ A.the var1) xs mean1R
      m2 :: Acc (AMatrix Float)
      m2 = A.zipWith (combine $ A.the var2) xs mean2R
      m' = A.generate (A.index1 n) $ \ i -> log $
        A.the mix * A.sfoldl (*) 1 i m1
        +
        (1 - A.the mix) * A.sfoldl (*) 1 i m2
  in
  A.sum m'
  where
    combine var x u = lhdNormalAcc u var x

llhdAccX :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAccX xs (A.unlift -> (mean1, var1, mean2, var2, mix)) =
  let lhd mean var (A.unlift . A.unindex2 -> (i,j)) = 
        lhdNormalAcc (mean A.! A.index1 j) var (xs A.! A.index2 i j)
      (n::Exp Int,_::Exp Int) = A.unlift $ A.unindex2 $ A.shape xs
      m1 :: Acc (AMatrix Float) 
      m1 = A.generate (A.shape xs) $ lhd mean1 $ A.the var1
      m2 :: Acc (AMatrix Float)
      m2 = A.generate (A.shape xs) $ lhd mean2 $ A.the var2
      m' = A.generate (A.index1 n) $ \ i -> log $
        A.the mix * A.sfoldl (*) 1 i m1
        +
        (1 - A.the mix) * A.sfoldl (*) 1 i m2
  in
  A.sum m'

llhdAcc1 :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAcc1 xs (A.unlift -> (mean1, var1, mean2, var2, mix)) = (p1 A.>-> p2 A.>-> p3 A.>-> A.sum) (A.unit 1)
  where
    p1 :: Acc (AScalar Int) -> Acc (AMatrix (Float, Float))
    p1 _ =
      let lhd mean var (A.unlift . A.unindex2 -> (i,j)) = 
            lhdNormalAcc (mean A.! A.index1 j) var (xs A.! A.index2 i j)
          m1 = A.generate (A.shape xs) $ lhd mean1 $ A.the var1
          m2 = A.generate (A.shape xs) $ lhd mean2 $ A.the var2
      in A.zip m1 m2
    p2 :: Acc (AMatrix (Float, Float)) -> Acc (AVector (Float, Float))
    p2 = A.fold f $ A.lift (1::Float,1::Float)
      where
        f (A.unlift -> (i1::Exp Float, i2::Exp Float)) (A.unlift -> (x1::Exp Float, x2::Exp Float)) = 
          A.lift (i1*x1, i2*x2)
    p3 :: Acc (AVector (Float, Float)) -> Acc (AVector Float)
    p3 = A.map $ \ (A.unlift -> (x1, x2)) -> log $ A.the mix * x1 + (1 - A.the mix) * x2

llhdAcc2 :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAcc2 xs = p1 A.>-> p2 A.>-> A.sum
  where
    p1 :: Acc ModelAcc -> Acc (AVector Float, AVector Float)
    p1 (A.unlift -> (mean1, var1, mean2, var2, mix)) =
      let lhd mean var (A.unlift . A.unindex2 -> (i,j)) = 
            lhdNormalAcc (mean A.! A.index1 j) var (xs A.! A.index2 i j)
          m1 = A.map (* A.the mix) $ A.fold (*) 1 $ A.generate (A.shape xs) $ lhd mean1 $ A.the var1
          m2 = A.map (* (1 - A.the mix)) $ A.fold (*) 1 $ A.generate (A.shape xs) $ lhd mean2 $ A.the var2
      in A.lift (m1, m2)
    p2 :: Acc (AVector Float, AVector Float) -> Acc (AVector Float)
    p2 (A.unlift -> (m1, m2)) = A.zipWith (\ x1 x2 -> log $ x1 + x2) m1 m2

-- doesn't work (nested parallelism)
llhdAcc3 :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAcc3 xs = p A.>-> A.sum
  where
    p :: Acc ModelAcc -> Acc (AVector Float)
    p (A.unlift -> (mean1, var1, mean2, var2, mix)) =
      let (n::Exp Int,_::Exp Int) = A.unlift $ A.unindex2 $ A.shape xs
      in 
      A.generate (A.index1 n) $ \ i ->
        let row :: Acc (AVector Float)
            row = A.slice xs $ A.lift (i A.:. A.All)
        in
        log $
        A.the mix * A.sfoldl (sfoldf $ A.the var1) 1 (A.constant A.Z) (A.zip row mean1)
        +
        (1 - A.the mix) * A.sfoldl (sfoldf $ A.the var2) 1 (A.constant A.Z) (A.zip row mean2)
    sfoldf :: Exp Float -> Exp Float -> Exp (Float, Float) -> Exp Float
    sfoldf var n (A.unlift -> (x, u)) = n * lhdNormalAcc u var x

llhdAccStupid :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAccStupid xs _ {-(A.unlift -> (mean1, var1, mean2, var2, mix))-} = A.unit (5::Exp Float)

llhdAccStupid1 :: Acc (AMatrix Float) -> Acc ModelAcc -> Acc (AScalar Float)
llhdAccStupid1 xs _ {-(A.unlift -> (mean1, var1, mean2, var2, mix))-} =
  A.product $ A.fold (+) 0 xs

------------------------------------------------------------
--------------- Full (joint) Model -------------------------
------------------------------------------------------------

makeJoint :: (ModelRepa -> Float) -> ModelRepa -> Float
makeJoint lhd model@(ModelRepa mean1 var1 mean2 var2 mix) =  
  lhd model
  * R.foldAllS (*) 1 (R.map (lhdNormal 0 1) mean1)
  * lhdGamma 1 1 var1
  * R.foldAllS (*) 1 (R.map (lhdNormal 0 1) mean2)
  * lhdGamma 1 1 var2
  * lhdNormal 0 1 (logit mix)

makeLJoint :: (ModelRepa -> Float) -> ModelRepa -> Float
makeLJoint llhd model@(ModelRepa mean1 var1 mean2 var2 mix) =
  llhd model
  + R.sumAllS (R.map (llhdNormal 0 1) mean1)
  + llhdGamma 1 1 var1
  + R.sumAllS (R.map (llhdNormal 0 1) mean2)
  + llhdGamma 1 1 var2
  + llhdNormal 0 1 (logit mix)

jointSerial :: RMatrixU Float -> ModelRepa -> Float
jointSerial = makeJoint . lhdSerial

ljointSerial :: RMatrixU Float -> ModelRepa -> Float
ljointSerial = makeLJoint . llhdSerial

jointPar :: RMatrixU Float -> ModelRepa -> Float
jointPar = makeJoint . lhdPar

ljointPar :: RMatrixU Float -> ModelRepa -> Float
ljointPar = makeLJoint . llhdPar

jointAcc :: RMatrixU Float -> ModelRepa -> Float
jointAcc xs = makeJoint lhd
  where
    lhd = accToScalar . step . modelRepaToAcc
    xsA = A.use $ A.fromRepa $ R.copyS xs
    step = A.run1 (lhdAcc xsA)

ljointAcc :: RMatrixU Float -> ModelRepa -> Float
ljointAcc xs = makeLJoint llhd
  where
    llhd = accToScalar . step . modelRepaToAcc
    xsA = A.use $ A.fromRepa $ R.copyS xs
    step = A.run1 (llhdAcc xsA)

ljointAcc' :: RMatrixU Float -> ModelRepa -> Float
ljointAcc' xs = makeLJoint llhd
  where
    llhd = accToScalar . step . modelRepaToAcc
    xsA = A.use $ A.fromRepa $ R.copyS xs
    step = A.run1 (llhdAcc' xsA)

ljointAcc'' :: RMatrixU Float -> ModelRepa -> Float
ljointAcc'' xs = makeLJoint llhd
  where
    llhd = accToScalar . step . modelRepaToAcc
    xsA = A.use $ A.fromRepa $ R.copyS xs
    step = A.run1 (llhdAcc'' xsA)

ljointAccX :: RMatrixU Float -> ModelRepa -> Float
ljointAccX xs = makeLJoint llhd
  where
    llhd = accToScalar . step . modelRepaToAcc
    xsA = A.use $ A.fromRepa $ R.copyS xs
    step = A.run1 (llhdAccX xsA)

ljointAcc1 :: RMatrixU Float -> ModelRepa -> Float
ljointAcc1 xs = makeLJoint llhd
  where
    llhd = accToScalar . step . modelRepaToAcc
    xsA = A.use $ A.fromRepa $ R.copyS xs
    step = A.run1 (llhdAcc1 xsA)

ljointAcc2 :: RMatrixU Float -> ModelRepa -> Float
ljointAcc2 xs = makeLJoint llhd
  where
    llhd = accToScalar . step . modelRepaToAcc
    xsA = A.use $ A.fromRepa $ R.copyS xs
    step = A.run1 (llhdAcc2 xsA)

ljointAccStupid :: RMatrixU Float -> ModelRepa -> Float
ljointAccStupid xs = makeLJoint llhd
  where
    llhd = accToScalar . step . modelRepaToAcc
    xsA = A.use $ A.fromRepa $ R.copyS xs
    step = A.run1 (llhdAccStupid1 xsA)

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

dim :: Int
dim = 3

-- for MCMC initialization
model0 :: ModelRepa
model0 = modelVectorToRepa $
  ModelVector (UV.replicate dim 0) 1 (UV.replicate dim 0) 1 0.5

-- for data generation
model1 :: ModelRepa
model1 = modelVectorToRepa $
  ModelVector (UV.replicate dim (-1)) 1 (UV.replicate dim 1) 10 0.33

------------------------------------------------------------
--------------- Synthetic Data -----------------------------
------------------------------------------------------------

genData :: Int -> IO ()
genData = genDataModel $ modelRepaToVector model1

genDataModel :: ModelVector -> Int -> IO ()
genDataModel model n = do
  vv <- liftM R.toUnboxed $ synthDataModel model n
  BS.writeFile "synthetic.data" $ encode (n, UV.length $ getMean1V model, vv)

synthData :: Int -> IO (RMatrixU Float)
synthData = synthDataModel $ modelRepaToVector model1

synthDataModel :: ModelVector -> Int -> IO (RMatrixU Float)
synthDataModel (ModelVector mean1 var1 mean2 var2 mix) n = withSystemRandom . asGenIO $ \ gen -> do
  liftM convertFromBoxed $ BV.replicateM n $ do
    u <- randomRIO (0::Float, 1)
    let (mean, var) = if u < mix
          then (mean1, var1)
          else (mean2, var2)
    UV.forM mean $ \ m ->
      liftM realToFrac $ genContVar (normalDistr (realToFrac m) $ realToFrac var) gen
  where
    convertFromBoxed :: VectorB (VectorU Float) -> RMatrixU Float
    convertFromBoxed v = 
      R.computeS $ 
      R.fromFunction (R.ix2 n $ UV.length mean1) $ \ (R.listOfShape -> [j,i]) -> 
        (v BV.! i) UV.! j

readData :: IO (RArrayU R.DIM2 Float)
readData = do
  Right (n::Int, m::Int, vv) <- liftM decode $ BS.readFile "synthetic.data"
  return $ R.fromUnboxed (R.ix2 n m) vv

dataSize :: IO R.DIM2
dataSize = do
  d <- readData
  return $ R.extent d

------------------------------------------------------------
--------------- Inference ----------------------------------
------------------------------------------------------------

modelMean :: (Monad m) => VectorB ModelRepa -> m ModelRepa
modelMean ms = do
  let n = BV.length ms
      nf = fromIntegral n
      [m] = R.listOfShape $ R.extent $ getMean1R $ ms BV.! 0
      mean1s = R.fromFunction (R.ix2 n m) $ \ (R.listOfShape -> [j,i]) ->
        getMean1R (ms BV.! i) R.! R.ix1 j
      var1s = R.fromFunction (R.ix1 n) $ \ (R.listOfShape -> [i]) ->
        getVar1R (ms BV.! i)
      mean2s = R.fromFunction (R.ix2 n m) $ \ (R.listOfShape -> [j,i]) ->
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

lsampleWith :: (ModelRepa -> Float) -> Int -> IO (VectorB ModelRepa, Int, ModelRepa, Float)
lsampleWith ljointPdf numIters = withSystemRandom . asGenIO $ \ gen ->
  lmh (modelProposal gen) ljointPdf model0 numIters

------------------------------------------------------------
--------------- TopLevel -----------------------------------
------------------------------------------------------------

lmainWith :: (RMatrixU Float -> ModelRepa -> Float) -> Int -> IO ()
lmainWith ljointPdf n = do
  xs <- readData
  (samples, successes, mx, mp) <- lsampleWith (ljointPdf xs) n
  avgx <- modelMean samples
  print "SUCCESSES"
  print successes
  print "MAP"
  print mx
  print "MAP-LLHD"
  print mp
  print "MODEL"
  print model1
  print "MODEL-LLHD"
  print $ ljointSerial xs model1
  print "MEAN"
  print avgx
  print "MEAN-LLHD"
  print $ ljointSerial xs avgx

lmainSerial :: Int -> IO ()
lmainSerial = lmainWith ljointSerial

lmainPar :: Int -> IO ()
lmainPar = lmainWith ljointPar

lmainAcc :: Int -> IO ()
lmainAcc = lmainWith ljointAcc

trueModelLlhd :: IO Float
trueModelLlhd = do
  xs <- readData
  return $ llhdSerial xs model1

fS :: RMatrixU Float -> Float -> Float
fS xs y = R.foldAllS (*) 1 $ R.foldS (+) y xs

fP :: RMatrixU Float -> Float -> Float
fP xs y = runIdentity $ do
  x <- R.foldP (+) y xs
  return $ R.foldAllS (*) 1 x

fA :: RMatrixU Float -> Float -> Float
fA xs = accToScalar . step . scalarToAcc
  where
    xsA = A.use $ A.fromRepa $ R.copyS xs
    comp :: Acc (AScalar Float) -> Acc (AScalar Float)
    comp y = 
      let (n::Exp Int,_::Exp Int) = A.unlift $ A.unindex2 $ A.shape xsA
      in A.product $ A.generate (A.index1 n) $ \ i -> A.sfoldl (+) (A.the y) i xsA 
      -- A.product $ A.fold (+) (A.the y) xsA
    step = A.run1 comp
