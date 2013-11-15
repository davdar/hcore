{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module FPUtil.Monad where

import Data.Function
import FPUtil.Function
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

type family MEnv (m :: * -> *) :: *
type family MOut (m :: * -> *) :: *
type family MState (m :: * -> *) :: *

type MonadReader' m = (MonadReader (MEnv m) m)
type MonadWriter' m = (MonadWriter (MOut m) m)
type MonadState' m = (MonadState (MState m) m)
