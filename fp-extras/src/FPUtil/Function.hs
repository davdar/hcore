module FPUtil.Function where

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.!) :: (b -> c) -> (a -> b) -> a -> c
(.!) g f x = g $! f x

applyTo :: a -> (a -> b) -> b
applyTo = flip id

