{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import System.Environment
import Control.DeepSeq
import System.IO
import System.IO.Unsafe
import Data.Vector.Cereal
import Data.Serialize (encode, decode)
import qualified Data.ByteString as BS
import Debug.Trace
import Text.Printf
import Control.Monad.Primitive
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC
import System.Random
import qualified Data.Array.Repa as R
import Control.Monad.State
import Data.Array.Accelerate (Acc, Scalar, Array, Arrays, Exp, Int32)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as A
import Data.IORef
import GHC.Exts
import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector as BVector
import qualified Data.Array.Accelerate.CUDA as AC

debug :: IORef Bool
{-# NOINLINE debug #-}
debug = unsafePerformIO $ newIORef False

log' :: String -> b -> b
log' msg x = unsafePerformIO $ do
  b <- readIORef debug
  when b $ do
    putStrLn msg
    hFlush stdout
  return x

trace' :: (Show a) => String -> a -> b -> b
trace' msg p x = unsafePerformIO $ do
  b <- readIORef debug
  when b $ do
    putStr msg
    putStrLn $ show p
    hFlush stdout
  return x

mlog' :: (Monad m) => String -> m ()
mlog' msg = log' msg $ return ()

mtrace' :: (Monad m, Show a) => String -> a -> m ()
mtrace' msg p = trace' msg p $ return ()

type RArrayA = R.Array A.A
type RArrayU = R.Array R.U

type BVector = BVector.Vector
type UVector = UVector.Vector

type MHState t = (t , Float, Int)

class VectorBij t where
  type VectorShape t :: *
  toV :: t -> UVector Float
  shapeV :: t -> VectorShape t
  fromV :: VectorShape t -> UVector Float -> t

mh :: forall t.
  (t -> IO t)
  -> (t -> Float)
  -> t
  -> Int
  -> IO (BVector t, Int)
mh propose pdf x0 iters0 = liftM extract $ flip runStateT i0 $ BVector.replicateM iters0 action
  where
    extract :: (BVector t, MHState t) -> (BVector t, Int)
    extract (r,(_,_,s)) = (r,s)
    i0 :: MHState t
    i0 = (x0, pdf x0, 0)
    action :: StateT (MHState t) IO t
    action = do
      (x,p,successes) <- get
      x' <- liftIO $ propose x
      let p' = pdf x'
          r = min 1 $ p' / p
      (u::Float) <- liftIO $ randomRIO (0, 1)
      --mtrace' "p=" p
      mlog' "."
      let (x'',p'',successes'') = if u < r
            then (x',p',successes+1)
            else (x,p,successes)
      put (x'',p'',successes'')
      return $ x''

mvGaussianProposal :: forall t. (VectorBij t) => Gen (PrimState IO) -> t -> IO t
mvGaussianProposal gen x = liftM (fromV $ shapeV x) $ UVector.generateM (UVector.length x') action
  where
    x' = toV x
    action :: Int -> IO Float
    action idx = liftM realToFrac $ flip genContVar gen $ normalDistr (realToFrac $ x' UVector.! idx) 1

type Model = 
  ( Array A.DIM1 Float  -- mean 1
  , Scalar Float        -- variance 1
  , Array A.DIM1 Float  -- mean 2
  , Scalar Float        -- variance 2
  , Scalar Float        -- mixture weight
  )

type ModelU =
  ( UVector Float
  , Float
  , UVector Float
  , Float
  , Float
  )

modelToU :: Model -> ModelU
modelToU (mean1, var1, mean2, var2, mix) = (mean1', var1', mean2', var2', mix')
  where
    mean1' = R.toUnboxed $ R.copyS $ A.toRepa mean1
    var1' = (R.toUnboxed $ R.copyS $ A.toRepa var1) UVector.! 0
    mean2' = R.toUnboxed $ R.copyS $ A.toRepa mean2
    var2' = (R.toUnboxed $ R.copyS $ A.toRepa var2) UVector.! 0
    mix' = (R.toUnboxed $ R.copyS $ A.toRepa mix) UVector.! 0

modelFromU :: ModelU -> Model
modelFromU (mean1, var1, mean2, var2, mix) = (mean1', var1', mean2', var2', mix')
  where
    mean1' = A.fromRepa $ R.copyS $ R.fromUnboxed (R.ix1 $ UVector.length mean1) mean1
    var1' = A.fromRepa $ R.copyS $ R.fromUnboxed R.Z $ UVector.singleton var1
    mean2' = A.fromRepa $ R.copyS $ R.fromUnboxed (R.ix1 $ UVector.length mean2) mean2
    var2' = A.fromRepa $ R.copyS $ R.fromUnboxed R.Z $ UVector.singleton var2
    mix' = A.fromRepa $ R.copyS $ R.fromUnboxed R.Z $ UVector.singleton mix

instance VectorBij Model where
  type VectorShape Model = VectorShape (Array A.DIM1 Float)
  toV (mean1, v1, mean2, v2, mix) = UVector.concat
    [ toV mean1
    , toV v1
    , toV mean2
    , toV v2
    , toV mix
    ]
  shapeV (mean1, _, _, _, _) = shapeV mean1
  fromV l v = 
    ( fromV l $ UVector.slice 0 l v
    , fromV () $ UVector.slice l 1 v
    , fromV l $ UVector.slice (l+1) l v
    , fromV () $ UVector.slice (2 * l + 1) 1 v
    , fromV () $ UVector.slice (2 * (l + 1)) 1 v
    )

instance VectorBij (Array A.DIM1 Float) where
  type VectorShape (Array A.DIM1 Float) = Int
  toV = R.toUnboxed . R.copyS . A.toRepa
  shapeV = R.size . R.extent . A.toRepa
  fromV i = A.fromRepa . R.copyS . R.fromUnboxed (R.Z R.:. i)

instance VectorBij (Scalar Float) where
  type VectorShape (Scalar Float) = ()
  toV = R.toUnboxed . R.copyS . A.toRepa
  shapeV = const ()
  fromV () = A.fromRepa . R.copyS . R.fromUnboxed R.Z

lhdNormalAcc :: Exp Float -> Exp Float -> Exp Float -> Exp Float
lhdNormalAcc mean variance x =
  let s = (x - mean) / variance
  in exp $ negate (1 / 2) * s * s

lhdGammaAcc :: Exp Float -> Exp Float -> Exp Float -> Exp Float
lhdGammaAcc shape scale x = (x ** (shape - 1)) * exp (negate $ x / scale)

pdfAcc :: Acc (Array A.DIM2 Float) -> Acc Model -> Acc (Scalar Float)
pdfAcc xs (A.unlift -> (mean1 , var1 , mean2 , var2 , mix)) =
  let lmix = 1 / (1 + (exp $ negate $ A.the mix))
      m1 = A.generate (A.shape xs) $ pdf_x_i mean1 (A.the var1) lmix
      m2 = A.generate (A.shape xs) $ pdf_x_i mean2 (A.the var2) (1 - lmix)
  in A.unit $
  A.the (A.product $ A.generate (A.shape mean1) $ \ i -> lhdNormalAcc 0 1 $ mean1 A.! i)
  * A.the (A.product $ A.generate (A.shape mean2) $ \ i -> lhdNormalAcc 0 1 $ mean2 A.! i)
  * lhdGammaAcc 1 1 (A.the var1 * A.the var1)
  * lhdGammaAcc 1 1 (A.the var2 * A.the var2)
  * lhdNormalAcc 0 1 (A.the mix)
  * A.the (A.product $ A.zipWith (+) m1 m2)
  where
    pdf_x_i :: Acc (Array A.DIM1 Float) -> Exp Float -> Exp Float -> Exp A.DIM2 -> Exp Float
    pdf_x_i mean var weight (A.unlift . A.unindex2 -> (i,j)) =
      weight * lhdNormalAcc (mean A.! A.index1 j) var (xs A.! A.index2 i j)

lhdNormal :: Float -> Float -> Float -> Float
lhdNormal mean variance x =
  let s = (x - mean) / variance
  in exp $ negate (1 / 2) * s * s

lhdGamma :: Float -> Float -> Float -> Float
lhdGamma shape scale x = (x ** (shape - 1)) * exp (negate $ x / scale)

pdfNative :: RArrayU R.DIM2 Float -> Model -> Float
pdfNative xs (modelToU -> (mean1U, var1, mean2U, var2, mix)) =
  let mean1 = R.fromUnboxed (R.ix1 $ UVector.length mean1U) mean1U
      mean2 = R.fromUnboxed (R.ix1 $ UVector.length mean2U) mean2U 
      lmix = 1 / (1 + negate mix)
      m1 = R.fromFunction (R.extent xs) $ pdf_x_i mean1 var1 lmix
      m2 = R.fromFunction (R.extent xs) $ pdf_x_i mean2 var2 (1 - lmix)
  in 
  (R.foldAllS (*) 1 $ R.map (lhdNormal 0 1) mean1)
  * (R.foldAllS (*) 1 $ R.map (lhdNormal 0 1) mean2)
  * lhdGamma 1 1 var1
  * lhdGamma 1 1 var2
  * lhdNormal 0 1 mix
  * (R.foldAllS (*) 1 $ R.zipWith (+) m1 m2)
  where
    pdf_x_i :: RArrayU R.DIM1 Float -> Float -> Float -> R.DIM2 -> Float
    pdf_x_i mean var weight (R.listOfShape -> [i,j]) =
      weight * lhdNormal (mean R.! R.ix1 j) var (xs R.! R.ix2 i j)

pdfCpuParallel :: RArrayU R.DIM2 Float -> Model -> Float
pdfCpuParallel xs (modelToU -> (mean1U, var1, mean2U, var2, mix)) =
  let mean1 = R.fromUnboxed (R.ix1 $ UVector.length mean1U) mean1U
      mean2 = R.fromUnboxed (R.ix1 $ UVector.length mean2U) mean2U 
      lmix = 1 / (1 + negate mix)
      m1 = R.fromFunction (R.extent xs) $ pdf_x_i mean1 var1 lmix
      m2 = R.fromFunction (R.extent xs) $ pdf_x_i mean2 var2 (1 - lmix)
  in unsafePerformIO $ do
  p1 <- R.foldAllP (*) 1 $ R.map (lhdNormal 0 1) mean1
  () <- p1 `seq` return ()
  p2 <- R.foldAllP (*) 1 $ R.map (lhdNormal 0 1) mean2
  () <- p2 `seq` return ()
  p3 <- R.foldAllP (*) 1 $ R.zipWith (+) m1 m2
  () <- p3 `seq` return ()
  return $
    p1 * p2 * p3
    * lhdGamma 1 1 var1
    * lhdGamma 1 1 var2
    * lhdNormal 0 1 mix
  where
    pdf_x_i :: RArrayU R.DIM1 Float -> Float -> Float -> R.DIM2 -> Float
    pdf_x_i mean var weight (R.listOfShape -> [i,j]) =
      weight * lhdNormal (mean R.! R.ix1 j) var (xs R.! R.ix2 i j)


runPdfWith :: (Arrays t) => (Acc t -> Acc (Scalar Float)) -> (forall a. (Arrays a) => Acc a -> a) -> t -> Float
runPdfWith f run m =
  head $ R.toList $ A.toRepa $ run $ f $ A.use m

smallData :: RArrayU R.DIM2 Float
smallData = R.fromListUnboxed (R.Z R.:. (5::Int) R.:. (3::Int))
  [ 1, 1, 1
  , 2, 2, 2
  , 1, 2, 1
  , 8, 8, 8
  , 8, 9, 8
  ]

mkPdfCuda :: RArrayU R.DIM2 Float -> Model -> Float
mkPdfCuda xs =
  let xs' :: Acc (Array A.DIM2 Float)
      xs' = A.use $ A.fromRepa $ R.copyS xs
      pdf = pdfAcc xs'
  in \ m ->
    head $ R.toList $ A.toRepa $ AC.run $ pdf (A.use m)

model0 :: Int -> ModelU
model0 n =
  ( UVector.replicate n 0
  , 1
  , UVector.replicate n 0
  , 1
  , 0.5
  )

model1 :: ModelU
model1 =
  ( UVector.fromList [-1,-1,-1]
  , 1
  , UVector.fromList [1,1,1]
  , 10
  , 0.5
  )

genData :: Int -> IO ()
genData = genDataModel model1

genDataModel :: ModelU -> Int -> IO ()
genDataModel (mean1, var1, mean2, var2, mix) n = withSystemRandom . asGenIO $ \ gen -> do
  v <- BVector.replicateM n $ do
    (u::Float) <- randomRIO (0, 1)
    let (mean, var) = if u < mix
          then (mean1, var1)
          else (mean2, var2)
    UVector.forM mean $ \ m ->
      liftM realToFrac $ genContVar (normalDistr (realToFrac m) $ realToFrac var) gen
  let a = convertFromBoxed v
      vv = R.toUnboxed a
  BS.writeFile "data" $ encode (n, UVector.length mean1, vv)
  where
    convertFromBoxed :: BVector (UVector Float) -> RArrayU R.DIM2 Float
    convertFromBoxed v = 
      R.computeS $ 
      R.fromFunction (R.ix2 n $ UVector.length mean1) $ \ (R.listOfShape -> [j,i]) -> 
        (v BVector.! i) UVector.! j

readData :: IO (RArrayU R.DIM2 Float)
readData = do
  Right (i, j, vv :: UVector Float) <- liftM decode $ BS.readFile "data"
  return $ R.fromUnboxed (R.ix2 i j) vv

dataSize :: IO R.DIM2
dataSize = do
  d <- readData
  return $ R.extent d

mainWith :: (RArrayU R.DIM2 Float -> Model -> Float) -> IO ()
mainWith mkPdf = withSystemRandom . asGenIO $ \ gen -> do
  xs <- readData
  let [_, cols] = R.listOfShape $ R.extent xs
      x0 :: Model
      x0 = modelFromU $ model0 cols
      numIters = 100
  (samples', successes) <- mh (mvGaussianProposal gen) (mkPdf xs) x0 numIters
  print "SUCCESSES"
  print successes
  let samples = BVector.map modelToU samples'
      toDrop = 0
  print $ 
    (/ (fromIntegral $ BVector.length samples - toDrop)) $ 
    BVector.sum $
    BVector.map (\(m,_,_,_,_) -> m UVector.! 0) $ 
    BVector.drop toDrop $ samples

runCuda :: IO ()
runCuda = mainWith mkPdfCuda

runNative :: IO ()
runNative = mainWith pdfNative

runCPUParallel :: IO ()
runCPUParallel = mainWith pdfCpuParallel

main :: IO ()
main = do
  [a] <- getArgs
  case a of
    "s" -> runNative
    "p" -> runCPUParallel
    "c" -> runCuda

-- testZig :: IO ()
-- testZig = do
--   print . AI.run . A.unit =<< evalGenIO zigNormalDefault
