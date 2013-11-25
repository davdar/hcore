module FP.Classes.Monad where

import Prelude ()

type family MEnv (m :: * -> *) :: *
type family MOut (m :: * -> *) :: *
type family MState (m :: * -> *) :: *

type MonadReader' m = (MonadReader (MEnv m) m)
type MonadWriter' m = (MonadWriter (MOut m) m)
type MonadState' m = (MonadState (MState m) m)

