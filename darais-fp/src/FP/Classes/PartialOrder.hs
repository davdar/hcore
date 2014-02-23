module FP.Classes.PartialOrder where

import Prelude ()
import FP.Data.Tuple
import FP.PrePrelude
import FP.Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- minimal completion is pcompare or lte
class PartialOrder t where
  pcompare :: t -> t -> Maybe Ordering
  pcompare x y = case (lte x y, lte y x) of
    (True, True) -> Just EQ
    (True, False) -> Just LT
    (False, True) -> Just GT
    (False, False) -> Nothing
  comparable :: t -> t -> Bool
  comparable x y = isJust $ pcompare x y
  lte :: t -> t -> Bool
  lte x y =
    let c = pcompare x y 
    in c == Just LT || c == Just EQ
  gte :: t -> t -> Bool
  gte x y =
    let c = pcompare x y
    in c == Just GT || c == Just EQ
  lt :: t -> t -> Bool
  lt x y = pcompare x y == Just LT
  gt :: t -> t -> Bool
  gt x y = pcompare x y == Just GT
  eq :: t -> t -> Bool
  eq x y = pcompare x y == Just EQ

instance PartialOrder () where 
  pcompare _ _ = Just EQ

instance PartialOrder Bool where
  pcompare x y = Just $ compare x y

instance PartialOrder Int where
  pcompare x y = Just $ compare x y

instance PartialOrder Integer where
  pcompare x y = Just $ compare x y

instance (PartialOrder a, PartialOrder b) => PartialOrder (a, b) where
  pcompare (a1,b1) (a2,b2) =
    case pcompare a1 a2 of
      Just LT -> Just LT
      Just EQ -> pcompare b1 b2
      Just GT -> Just GT
      Nothing -> Nothing

instance 
  (PartialOrder a, PartialOrder b, PartialOrder c) 
  => PartialOrder (a, b, c) 
  where
    pcompare = pcompare `on` group3

instance
  (PartialOrder a, PartialOrder b, PartialOrder c, PartialOrder d)
  => PartialOrder (a, b, c, d)
  where
    pcompare = pcompare `on` group4

instance (PartialOrder a, PartialOrder b) => PartialOrder (Either a b) where
  pcompare (Left a1) (Left a2) = pcompare a1 a2
  pcompare (Left _) (Right _) = Just LT
  pcompare (Right _) (Left _) = Just GT
  pcompare (Right b1) (Right b2) = pcompare b1 b2

instance (Ord a) => PartialOrder (Set a) where
  lte = Set.isSubsetOf

instance (Ord k, PartialOrder v) => PartialOrder (Map k v) where
  lte = Map.isSubmapOfBy lte
