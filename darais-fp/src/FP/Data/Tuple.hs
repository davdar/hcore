module FP.Data.Tuple
  ( module FP.Data.Tuple
  , module Data.Tuple
  ) where

import Data.Tuple
import Control.Monad

group3 :: (a,b,c) -> ((a,b),c)
group3 (a,b,c) = ((a,b),c)

ungroup3 :: ((a,b),c) -> (a,b,c)
ungroup3 ((a,b),c) = (a,b,c)

group4 :: (a,b,c,d) -> ((a,b),c,d)
group4 (a,b,c,d) = ((a,b),c,d)

ungroup4 :: ((a,b),c,d) -> (a,b,c,d)
ungroup4 ((a,b),c,d) = (a,b,c,d)

firstM :: (Monad m) => (a -> m c) -> (a,b) -> m (c,b)
firstM f (a,b) = liftM (,b) $ f a

secondM :: (Monad m) => (b -> m c) -> (a,b) -> m (a,c)
secondM f (a,b) = liftM (a,) $ f b
