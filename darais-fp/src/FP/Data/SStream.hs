module FP.Data.SStream where

import Prelude ()
import FP.Data.Nat
import FP.Data.Stream

newtype SStream (i::Nat) m a = SStream { unSStream :: Stream m a }
