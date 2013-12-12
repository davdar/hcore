module Stats where

import Control.Monad.Primitive
import Control.Monad.State
import Data.Array.Accelerate (Exp)
import Statistics.Distribution hiding (mean, variance)
import Statistics.Distribution.Normal
import System.Random
import System.Random.MWC
import Types
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV

logistic :: Float -> Float
logistic x = 1 / (1 + (exp $ negate x))

logit :: Float -> Float
logit x = negate $ log (1 / x - 1)

lhdNormal :: Float -> Float -> Float -> Float
lhdNormal mean variance x =
  let s = (x - mean) / variance
  in exp $ negate (1 / 2) * s * s

llhdNormal :: Float -> Float -> Float -> Float
llhdNormal mean variance x = 
  let s = (x - mean) / variance
  in negate (1 / 2) * s * s

lhdNormalAcc :: Exp Float -> Exp Float -> Exp Float -> Exp Float
lhdNormalAcc mean variance x =
  let s = (x - mean) / variance
  in exp $ negate (1 / 2) * s * s

lhdGamma :: Float -> Float -> Float -> Float
lhdGamma shape scale x = (x ** (shape - 1)) * exp (negate $ x / scale)

llhdGamma :: Float -> Float -> Float -> Float
llhdGamma shape scale x = (shape - 1) * log x + (negate $ x / scale)

lhdGammaAcc :: Exp Float -> Exp Float -> Exp Float -> Exp Float
lhdGammaAcc shape scale x = (x ** (shape - 1)) * exp (negate $ x / scale)

mvGaussianProposal :: Gen (PrimState IO) -> VectorU Float -> IO (VectorU Float)
mvGaussianProposal gen x = UV.generateM (UV.length x) action
  where
    action :: Int -> IO Float
    action idx = liftM realToFrac $ flip genContVar gen $ normalDistr (realToFrac $ x UV.! idx) 1

mh :: forall t. (t -> IO t) -> (t -> Float) -> t -> Int -> IO (VectorB t, Int, t, Float)
mh propose pdf x0 iters0 = liftM extract $ flip runStateT i0 $ BV.replicateM iters0 action
  where
    extract :: (VectorB t, (t, Float, Int, t, Float)) -> (VectorB t, Int, t, Float)
    extract (r,(_,_,s,mx,mp)) = (r,s,mx,mp)
    i0 :: (t, Float, Int, t, Float)
    i0 = let pdfx0 = pdf x0 in (x0, pdfx0, 0, x0, pdfx0)
    action :: StateT (t, Float, Int, t, Float) IO t
    action = do
      (x,p,successes,mx,mp) <- get
      x' <- liftIO $ propose x
      let p' = pdf x'
          r = min 1 $ p' / p
      u <- liftIO $ randomRIO (0::Float, 1)
      let (x'',p'',s'') = if u < r
            then (x',p',succ successes)
            else (x,p,successes)
          (mx'',mpdf'') = if p' > mp
            then (x',p')
            else (mx,mp)
      put $ mx'' `seq` (x'',p'',s'',mx'',mpdf'')
      return x''

lmh :: forall t. (t -> IO t) -> (t -> Float) -> t -> Int -> IO (VectorB t, Int, t, Float)
lmh propose lpdf x0 iters0 = liftM extract $ flip runStateT i0 $ BV.replicateM iters0 action
  where
    extract :: (VectorB t, (t, Float, Int, t, Float)) -> (VectorB t, Int, t, Float)
    extract (r,(_,_,s,mx,mp)) = (r,s,mx,mp)
    i0 :: (t, Float, Int, t, Float)
    i0 = let lpdfx0 = lpdf x0 in (x0, lpdfx0, 0, x0, lpdfx0)
    action :: StateT (t, Float, Int, t, Float) IO t
    action = do
      (x,lp,successes,mx,mp) <- get
      x' <- liftIO $ propose x
      let lp' = lpdf x'
          lr = min 0 $ lp' - lp
      u <- liftIO $ randomRIO (0::Float, 1)
      let (x'',p'',s'') = if log u < lr
            then (x',lp',succ successes)
            else (x,lp,successes)
          (mx'',mpdf'') = if lp' > mp
            then (x',lp')
            else (mx,mp)
      put $ mx'' `seq` (x'',p'',s'',mx'',mpdf'')
      return x''
