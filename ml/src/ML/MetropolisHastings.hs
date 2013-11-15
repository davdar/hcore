{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ML.MetropolisHastings where

import Control.Arrow
import Data.Monoid
import Classes.Accessible
import Control.Monad
import Statistics.Distribution.Uniform
import ML.Statistics
import ML.MonadGen
import qualified Data.V.Boxed as V.B
import qualified Data.V.Prim as V
import Data.V.Prim (V)
import Data.M.Prim (M)
import qualified Data.M.Prim as M
import Data.Nat
import Control.Monad.Writer

mhIter :: (MonadGen m, MonadWriter (Sum Int) m) 
  => (V j Double -> Double) 
  -> (V j Double -> m (V j Double)) 
  -> V j Double 
  -> m (V j Double)
mhIter p qSampler x = do
  x' <- qSampler x
  let r = min 1 $ p x' / p x
  u <- genContVarM $ uniformDistr 0 1
  let b = u < r
  when b $ tell $ Sum 1
  return $ if b then x' else x

mh :: forall i j m. (MonadGen m) 
  => (V j Double -> Double) 
  -> (V j Double -> m (V j Double)) 
  -> V j Double 
  -> SInt i 
  -> m (M i j Double, Int)
mh p qSampler x iS = liftM (second getSum) $ runWriterT $ do
  samples <- V.B.iterateNM iS (mhIter p (lift . qSampler)) x
  return $ M.generate iS (V.length x) $ \ (iB, jB) -> samples ! iB ! jB

randomCentered :: (MonadGen m) => Double -> Double -> m (Double, Double)
randomCentered center width = do
  r <- genContVarM $ uniformDistr 0 width
  let lo = center - r
      hi = lo + width
  -- liftIO $ putStrLn $ printf "center: %s width: %s lo: %s hi: %s" (show center) (show width) (show lo) (show hi)
  return (lo, hi)

sliceIter :: (MonadGen m) => Double -> (V j Double -> Double) -> BInt j -> V j Double -> m (V j Double)
sliceIter width p jB xV = do
  -- liftIO $ putStr "."
  y <- genContVarM $ uniformDistr 0 $ p xV
  (lo, hi) <- randomCentered (xV ! jB) width
  stepOutLeft y lo hi
  where
    stepOutLeft y lo hi = do
      let xV' = V.updateIndex xV jB lo
      -- liftIO $ putStrLn $ printf "left: %s - %s" (show lo) (show hi)
      -- liftIO $ putStrLn $ printf "old: %s" $ show xV
      -- liftIO $ putStrLn $ printf "new: %s" $ show xV'
      -- liftIO $ putStrLn $ printf "y: %s" $ show y
      -- liftIO $ putStrLn $ printf "p new: %s" $ show $ p xV'
      if y < p xV'
        then stepOutLeft y (lo - width) hi
        else stepOutRight y lo hi
    stepOutRight y lo hi = do
      let xV' = V.updateIndex xV jB hi
      -- liftIO $ putStrLn $ printf "right: %s - %s" (show lo) (show hi)
      -- liftIO $ putStrLn $ printf "old: %s" $ show xV
      -- liftIO $ putStrLn $ printf "new: %s" $ show xV'
      -- liftIO $ putStrLn $ printf "y: %s" $ show y
      -- liftIO $ putStrLn $ printf "p new: %s" $ show $ p xV'
      if y < p xV'
        then stepOutRight y lo (hi + width)
        else shrink y lo hi
    shrink y lo hi = do
      xj <- genContVarM $ uniformDistr lo hi
      let xV' = V.updateIndex xV jB xj
      -- liftIO $ putStrLn $ printf "shrink: %s - %s" (show lo) (show hi)
      -- liftIO $ putStrLn $ printf "old: %s" $ show xV
      -- liftIO $ putStrLn $ printf "new: %s" $ show xV'
      -- liftIO $ putStrLn $ printf "y: %s" $ show y
      -- liftIO $ putStrLn $ printf "p new: %s" $ show $ p xV'
      if y <= p xV'
        then return xV'
        else if xj < (xV ! jB)
          then shrink y xj hi
          else shrink y lo xj

sliceMh :: (MonadGen m) => (V j Double -> Double) -> V j Double -> SInt i -> m (M i j Double)
sliceMh p x iS = do
  samples <- flip (V.B.iterateNM iS) x $ iterateBIntM (V.length x) $ sliceIter 100.0 p 
  return $ M.generate iS (V.length x) $ \ (iB, jB) -> samples ! iB ! jB
