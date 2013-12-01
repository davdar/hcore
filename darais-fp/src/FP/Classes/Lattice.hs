module FP.Classes.Lattice where

import Prelude ()
import FP.PrePrelude
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import FP.Data.Function
import FP.Data.Tuple

class Lattice a where
  lbot :: a
  ltop :: a
  ljoin :: a -> a -> a
  lmeet :: a -> a -> a

instance Lattice () where
  lbot = ()
  ltop = ()
  ljoin _ _ = ()
  lmeet _ _ = ()

instance (Lattice a, Lattice b) => Lattice (Either a b) where
  lbot = Left lbot
  ltop = Right ltop
  ljoin (Left a) (Left b) = Left $ ljoin a b
  ljoin (Left _) x@(Right _) = x
  ljoin x@(Right _) (Left _) = x
  ljoin (Right a) (Right b) = Right $ ljoin a b
  lmeet (Left a) (Left b) = Left $ lmeet a b
  lmeet x@(Left _) (Right _) = x
  lmeet (Right _) x@(Left _) = x
  lmeet (Right a) (Right b) = Right $ lmeet a b

instance (Lattice a, Lattice b) => Lattice (a,b) where
  lbot = (lbot,lbot)
  ltop = (ltop,ltop)
  ljoin (a1,b1) (a2,b2) = (ljoin a1 a2,ljoin b1 b2)
  lmeet (a1,b1) (a2,b2) = (lmeet a1 a2,lmeet b1 b2)

instance 
  (Lattice a, Lattice b, Lattice c) 
  => Lattice (a, b, c) 
  where
    lbot = ungroup3 lbot
    ltop = ungroup3 ltop
    ljoin = ungroup3 .: ljoin `on` group3
    lmeet = ungroup3 .: lmeet `on` group3

instance
  (Lattice a, Lattice b, Lattice c, Lattice d)
  => Lattice (a, b, c, d)
  where
    lbot = ungroup4 lbot
    ltop = ungroup4 ltop
    ljoin = ungroup4 .: ljoin `on` group4
    lmeet = ungroup4 .: lmeet `on` group4

instance (Ord a) => Lattice (Set a) where
  lbot = Set.empty
  ltop = error "no representation of top set"
  ljoin = Set.union
  lmeet = Set.intersection

instance (Ord k, Lattice v) => Lattice (Map k v) where
  lbot = Map.empty
  ltop = error "no representation of top map"
  ljoin = Map.unionWith ljoin
  lmeet = Map.intersectionWith lmeet

lmeets :: (Lattice l) => [l] -> l
lmeets = foldr lmeet ltop

ljoins :: (Lattice l) => [l] -> l
ljoins = foldr ljoin lbot
