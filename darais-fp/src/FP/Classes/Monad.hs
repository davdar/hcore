module FP.Classes.Monad 
  ( module FP.Classes.Monad
  , module Control.Monad.Error
  , module Control.Monad.Trans.Either
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Control.Monad.Writer
  , module Control.Monad.Identity
  , module Control.Monad.RWS
  , module Control.Monad.Maybe
  , module Control.Monad
  ) where

import Prelude ()
import FP.PrePrelude
import Control.Monad hiding (sequence, mapM)
import Control.Monad.State hiding (sequence, mapM)
import Control.Monad.Reader hiding (sequence, mapM)
import Control.Monad.Writer hiding (sequence, mapM)
import Control.Monad.Identity hiding (sequence, mapM)
import Control.Monad.RWS hiding (sequence, mapM)
import Control.Monad.Error hiding (sequence, mapM)
import Control.Monad.Trans.Either
import Control.Monad.Maybe

type family MEnv (m :: * -> *) :: *
type family MOut (m :: * -> *) :: *
type family MState (m :: * -> *) :: *

type MonadReader' m = (MonadReader (MEnv m) m)
type MonadWriter' m = (MonadWriter (MOut m) m)
type MonadState' m = (MonadState (MState m) m)
