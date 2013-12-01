module FP.Classes.Compat where

import GHC.Exts

type family Compat (t :: * -> *) :: * -> Constraint

class Universal t where
instance Universal t where
