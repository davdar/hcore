module FPUtil.Functor where

each :: (Functor t) => t a -> (a -> b) -> t b
each = flip fmap
