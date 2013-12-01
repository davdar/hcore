module FP.Data.Either where

import Prelude ()
import FP.PrePrelude

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight _ (Left x) = Left x
mapRight f (Right x) = Right $ f x

