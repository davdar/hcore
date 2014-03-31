module FP.Data.DumbLattice where

-- TODO: rename this to 'CustomLattice' or 'ConstraintLattice'
-- TODO: make a proper merge function for merging two DumbLattices
--   (Map.unionWith isn't correct because it isn't transitive)

import Prelude ()
import FP.PrePrelude
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import FP.Classes.Sequence
import FP.Data.List ()

-- (x, y) specifies x <= y
type Constraint = (String, String)

data Level = BotLevel | TopLevel | Level Int String -- higher Int means lower in the lattice!
  deriving (Eq, Ord, Show)

level :: String -> Level
level = Level 0

bumpLevel :: Level -> Level
bumpLevel BotLevel = BotLevel
bumpLevel TopLevel = TopLevel
bumpLevel (Level i x) = Level (i+1) x

-- the transitive closure of the <= relation
type DumbLattice = Map String (Set String)

dlCompare :: DumbLattice -> Level -> Level -> Maybe Ordering
dlCompare _ BotLevel _ = Just LT
dlCompare _ _ BotLevel = Just GT
dlCompare _ TopLevel _ = Just GT
dlCompare _ _ TopLevel = Just LT
dlCompare dl (Level i1 s1) (Level i2 s2) = dlCompareLevel dl i1 s1 i2 s2

dlLte :: DumbLattice -> Level -> Level -> Bool
dlLte dl l1 l2 = case dlCompare dl l1 l2 of
  Just LT -> True
  Just EQ -> True
  _ -> False

dlCompareLevel :: DumbLattice -> Int -> String -> Int -> String -> Maybe Ordering
dlCompareLevel dl i1 s1 i2 s2 = case dlCompareString dl s1 s2 of
  Nothing -> Nothing
  Just LT -> Just LT
  Just EQ -> Just $ compare i2 i1
  Just GT -> Just GT

dlCompareString :: DumbLattice -> String -> String -> Maybe Ordering
dlCompareString dl s1 s2 =
  let ss1 = fromMaybe (Set.singleton s1) $ Map.lookup s1 dl
      ss2 = fromMaybe (Set.singleton s2) $ Map.lookup s2 dl
  in case (s2 `Set.member` ss1, s1 `Set.member` ss2) of
    (True, True) -> Just EQ
    (True, False) -> Just LT
    (False, True) -> Just GT
    (False, False) -> Nothing

compile :: [Constraint] -> DumbLattice
compile = siterFrom Map.empty $ \ (x,y) db ->
  let db' = Map.insertWith Set.union x (Set.singleton x) db
  in flip Map.map db' $ \ ys ->
    if x `Set.member` ys then Set.insert y ys else ys
