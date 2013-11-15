{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Classes.CFunctor where

import GHC.Exts

class CFunctor t where
  type Compat t :: * -> Constraint
  cmap :: (Compat t a, Compat t b) => (a -> b) -> t a -> t b


