module FP.Data.Indexed (Indexed) where

import Prelude ()
import FP.Classes.Static

newtype Indexed t (i::k) = Indexed { unIndexed :: t }

instance Static (Indexed t::k -> *) where
  type Stripped (Indexed t::k -> *) = t  
  stripS = unIndexed
  unsafeS = Indexed

