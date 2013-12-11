module Util where

import Control.DeepSeq
import Control.Monad
import Data.IORef
import System.IO
import System.IO.Unsafe
import Types
import qualified Data.Vector.Unboxed as UV
import qualified Data.Array.Accelerate.IO as A
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Repa as R

debug :: IORef Bool
{-# NOINLINE debug #-}
debug = unsafePerformIO $ newIORef True

log' :: String -> b -> b
log' msg x = unsafePerformIO $ do
  b <- readIORef debug
  when b $ do
    putStrLn msg
    hFlush stdout
  return x

mlog' :: (Monad m) => String -> m ()
mlog' msg = log' msg $ return ()

trace' :: (Show a) => String -> a -> b -> b
trace' msg p x = unsafePerformIO $ do
  b <- readIORef debug
  when b $ do
    putStr msg
    putStrLn $ show p
    hFlush stdout
  return x

mtrace' :: (Monad m, Show a) => String -> a -> m ()
mtrace' msg p = trace' msg p $ return ()

accToVector :: (UV.Unbox a, A.Elt a) => AVector a -> VectorU a
accToVector = repaToVector . R.copyS . A.toRepa

vectorToAcc :: (UV.Unbox a, A.Elt a) => VectorU a -> AVector a
vectorToAcc = A.fromRepa . R.copyS . vectorToRepa

accToScalar :: (UV.Unbox a, A.Elt a) => AScalar a -> a
accToScalar = UV.head . R.toUnboxed . R.copyS . A.toRepa

scalarToAcc :: (UV.Unbox a, A.Elt a) => a -> AScalar a
scalarToAcc = A.fromRepa . R.copyS . R.fromUnboxed R.Z . UV.singleton

repaToAcc :: (UV.Unbox a, A.Elt a) => RVectorU a -> AVector a
repaToAcc = A.fromRepa . R.copyS

accToRepa :: (UV.Unbox a, A.Elt a) => AVector a -> RVectorU a
accToRepa = R.copyS . A.toRepa

repaToVector :: (UV.Unbox a) => RVectorU a -> VectorU a
repaToVector = R.toUnboxed

vectorToRepa :: (UV.Unbox a) => VectorU a -> RVectorU a
vectorToRepa v = R.fromUnboxed (R.ix1 $ UV.length v) v

instance (UV.Unbox a) => NFData (RVectorU a) where
  rnf v = v `R.deepSeqArray` ()
