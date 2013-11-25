module FP.Data.SList (SList) where

import Prelude ()
import FP.Classes.Eq
import FP.Classes.Ord
import FP.Pretty
import FP.Data.Nat

newtype SList (i::Nat) a = SList { unSList :: [a] }
  deriving (Eq, Ord, Pretty)
