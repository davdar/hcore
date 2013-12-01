module FP.Data.List where

import Prelude ()
import FP.PrePrelude
import FP.Classes.Monad
import FP.Classes.Functor
import FP.Classes.Sequence
import FP.Data.Stream (Stream, stream)
import qualified FP.Data.Stream as S
import qualified Data.List as List
import FP.Classes.Compat

type instance Compat [] = Universal

instance TFunctor [] where
  mapM = _mapM

instance CFunctor [] where
  cmapM = mapM

instance Sequence [] where
  toStream = _toStream
  fromStream = _fromStream
  length = List.length
  (!) = (List.!!)

_mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
_mapM f = loop
  where
    loop [] = return []
    loop (x:xs) = do
      y <- f x
      liftM (y:) $ loop xs

_toStream ::[a] -> Stream a
_toStream xs0 = stream xs0 $ step
  where
    step [] = S.Done
    step (x:xs) = S.Yield x xs

_fromStream :: Int -> Stream a -> [a]
_fromStream _ = S.iterR (:) []

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ [] = []
mapFirst f (x:xs) = f x:xs

mapRest :: (a -> a) -> [a] -> [a]
mapRest _ [] = []
mapRest f (x:xs) = x:map f xs

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f [x] = [f x]
mapLast f (x:xs) = x:mapLast f xs

mapLeading :: (a -> a) -> [a] -> [a]
mapLeading _ [] = []
mapLeading _ [x] = [x]
mapLeading f (x:xs) = f x:mapLeading f xs
