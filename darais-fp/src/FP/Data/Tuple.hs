module FP.Data.Tuple
  ( module FP.Data.Tuple
  , module Data.Tuple
  ) where

import Data.Tuple

group3 :: (a,b,c) -> ((a,b),c)
group3 (a,b,c) = ((a,b),c)

ungroup3 :: ((a,b),c) -> (a,b,c)
ungroup3 ((a,b),c) = (a,b,c)

group4 :: (a,b,c,d) -> ((a,b),c,d)
group4 (a,b,c,d) = ((a,b),c,d)

ungroup4 :: ((a,b),c,d) -> (a,b,c,d)
ungroup4 ((a,b),c,d) = (a,b,c,d)

