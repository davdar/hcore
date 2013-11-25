module FP.Data.Bool where

import Prelude ()

cond :: (a -> Bool) -> c -> c -> a -> c
cond f isT isF x = if f x then isT else isF

