{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Debug.Trace
import Text.Printf
import Control.Monad.Primitive
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC
import System.Random
import Zig
import Gen
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

mh :: forall t. (Arrays t, Show t)
  => (forall a. (A.Arrays a) => Acc a -> a)
  -> (t -> IO t)
  -> (Acc t -> Acc (Scalar Float))
  -> t
  -> Int
  -> IO (BVector t, Int)
mh run propose pdf x0 iters0 = liftM extract $ flip runStateT i0 $ BVector.replicateM iters0 action
  where
    extract :: (BVector t, MHState t) -> (BVector t, Int)
    extract (r,(_,_,s)) = (r,s)
    i0 :: MHState t
    i0 = (x0, head $ A.toList $ run $ pdf $ A.use x0, 0)
    action :: StateT (MHState t) IO t
    action = do
      (x,p,successes) <- get
      x' <- liftIO $ propose x
      let p' = head $ A.toList $ run $ pdf $ A.use x'
          r = min 1 $ p' / p
      --() <- return $ flip trace () $ printf "x=%s" $ show x
      --() <- return $ flip trace () $ printf "x'=%s" $ show x'
      (u::Float) <- liftIO $ randomRIO (0, 1)
      when (p > 0) $ do
        () <- return $ flip trace () $ printf "p=%f" p
        () <- return $ flip trace () $ printf "p'=%f" p'
        () <- return $ flip trace () $ printf "r=%f" r
        () <- return $ flip trace () $ printf "u=%f" u
        return ()
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

lhdNormal :: Exp Float -> Exp Float -> Exp Float -> Exp Float
lhdNormal mean variance x =
  let s = (x - mean) / variance
  in exp $ negate (1 / 2) * s * s

lhdGamma :: Exp Float -> Exp Float -> Exp Float -> Exp Float
lhdGamma shape scale x = (x ** (shape - 1)) * exp (negate $ x / scale)

pdf :: Acc (Array A.DIM2 Float) -> Acc Model -> Acc (Scalar Float)
pdf xs (A.unlift -> (mean1 , var1 , mean2 , var2 , mix)) =
  let lmix = 1 / (1 + (exp $ negate $ A.the mix))
      m1 = A.generate (A.shape xs) $ pdf_x_i mean1 (A.the var1) lmix
      m2 = A.generate (A.shape xs) $ pdf_x_i mean2 (A.the var2) (1 - lmix)
  in A.unit $
    A.the (A.product $ A.generate (A.shape mean1) $ \ i -> lhdNormal 0 1 $ mean1 A.! i)
  * A.the (A.product $ A.generate (A.shape mean2) $ \ i -> lhdNormal 0 1 $ mean1 A.! i)
  * lhdGamma 1 1 (A.the var1 * A.the var1)
  * lhdGamma 1 1 (A.the var2 * A.the var2)
  * lhdNormal 0 1 (A.the mix)
  * A.the (A.product $ A.zipWith (+) m1 m2)
  where
    pdf_x_i :: Acc (Array A.DIM1 Float) -> Exp Float -> Exp Float -> Exp A.DIM2 -> Exp Float
    pdf_x_i mean var weight (A.unlift . A.unindex2 -> (i,j)) =
      weight * lhdNormal (mean A.! A.index1 j) var (xs A.! A.index2 i j)

smallData :: RArrayU R.DIM2 Float
smallData = R.fromListUnboxed (R.Z R.:. (5::Int) R.:. (3::Int))
  [ 1, 1, 1
  , 2, 2, 2
  , 1, 2, 1
  , 8, 8, 8
  , 8, 9, 8
  ]

main :: IO ()
main = withSystemRandom . asGenIO $ \ gen -> do
  let raw :: RArrayU R.DIM2 Float
      raw = smallData
      [rows, cols] = R.listOfShape $ R.extent raw
      xs :: Acc (Array A.DIM2 Float)
      xs = A.use $ A.fromRepa $ R.copyS raw
      x0 :: Model
      x0 = modelFromU
        ( UVector.replicate cols 0
        , 1
        , UVector.replicate cols 0
        , 1
        , 0.5
        )
  (samples', successes) <- mh AI.run (mvGaussianProposal gen) (pdf xs) x0 1000
  print "SUCCESSES"
  print successes
  let samples = BVector.map modelToU samples'
  print $ 
    (/ (fromIntegral $ BVector.length samples - 1000)) $ 
    BVector.sum $
    BVector.map (\(m,_,_,_,_) -> m UVector.! 0) $ 
    BVector.drop 1000 $ samples
  -- print "SAMPLES"
  -- print samples

testZig :: IO ()
testZig = do
  print . AI.run . A.unit =<< evalGenIO zigNormalDefault
