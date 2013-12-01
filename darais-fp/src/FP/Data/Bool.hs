module FP.Data.Bool where

cond :: (a -> Bool) -> b -> b -> a -> b
cond c tb fb a = if c a then tb else fb
