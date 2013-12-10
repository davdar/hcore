module Stats where

import System.Random
import Control.Monad.State
import Statistics.Distribution hiding (mean, variance)
import Statistics.Distribution.Normal
import Data.Array.Accelerate (Exp)
import System.Random.MWC
import Control.Monad.Primitive
import Types
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as BV

logistic :: Float -> Float
logistic x = 1 / (1 + (exp $ negate x))

logit :: Float -> Float
logit x = negate $ log (1 / x - 1)

lhdNormal :: Float -> Float -> Float -> Float
lhdNormal mean variance x =
  let s = (x - mean) / variance
  in exp $ negate (1 / 2) * s * s

lhdNormalAcc :: Exp Float -> Exp Float -> Exp Float -> Exp Float
lhdNormalAcc mean variance x =
  let s = (x - mean) / variance
  in exp $ negate (1 / 2) * s * s

lhdGamma :: Float -> Float -> Float -> Float
lhdGamma shape scale x = (x ** (shape - 1)) * exp (negate $ x / scale)

lhdGammaAcc :: Exp Float -> Exp Float -> Exp Float -> Exp Float
lhdGammaAcc shape scale x = (x ** (shape - 1)) * exp (negate $ x / scale)

mvGaussianProposal :: Gen (PrimState IO) -> VectorU Float -> IO (VectorU Float)
mvGaussianProposal gen x = UV.generateM (UV.length x) action
  where
    action :: Int -> IO Float
    action idx = liftM realToFrac $ flip genContVar gen $ normalDistr (realToFrac $ x UV.! idx) 1

mh :: forall t. (t -> IO t) -> (t -> Float) -> t -> Int -> IO (VectorB t, Int)
mh propose pdf x0 iters0 = liftM extract $ flip runStateT i0 $ BV.replicateM iters0 action
  where
    extract :: (VectorB t, (t, Float, Int)) -> (VectorB t, Int)
    extract (r,(_,_,s)) = (r,s)
    i0 :: (t, Float, Int)
    i0 = (x0, pdf x0, 0)
    action :: StateT (t, Float, Int) IO t
    action = do
      (x,p,successes) <- get
      x' <- liftIO $ propose x
      let p' = pdf x'
          r = min 1 $ p' / p
      u <- liftIO $ randomRIO (0::Float, 1)
      let st@(x'',_,_) = if u < r
            then (x',p',succ successes)
            else (x',p,successes)
      put st
      return x''
