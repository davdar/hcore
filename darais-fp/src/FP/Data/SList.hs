module FP.Data.SList (SList) where

import Prelude ()
import FP.PrePrelude
import FP.Classes.Static
import FP.Pretty
import FP.Data.Nat

newtype SList (i::Nat) a = SList { unSList :: [a] }
  deriving (Eq, Ord, Pretty)

instance Static1 SList where
  type Stripped1 SList = []
  stripS1 = unSList
  unsafeS1 = SList
