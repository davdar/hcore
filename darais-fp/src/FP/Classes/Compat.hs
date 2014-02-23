module FP.Classes.Compat where

import GHC.Exts

-- A way of specifying constraints on the domain of a functor
type family Compat (t :: * -> *) :: * -> Constraint

class Universal t where
instance Universal t where
