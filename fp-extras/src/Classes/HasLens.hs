{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Classes.HasLens where

import FPUtil.Monad
import Control.Arrow
import Prelude hiding (id, (.))
import Control.Category
import Data.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

class HasLens a b where
  view :: Lens a b

instance HasLens a a where
  view = iso id id

type MonadReaderView b m = (MonadReader' m, HasLens (MEnv m) b)

askView :: (MonadReaderView b m) => Lens b c -> m c
askView l = liftM (getL $ l . view) ask

localViewMod :: (MonadReaderView b m) => Lens b c -> (c -> c) -> m a -> m a
localViewMod l = local . modL (l . view)

localViewSet :: (MonadReaderView b m) => Lens b c -> c -> m a -> m a
localViewSet l = local . setL (l . view)

type MonadWriterView b m = (MonadWriter' m, HasLens (MOut m) b)

tellView :: (MonadWriterView b m) => Lens b c -> c -> m ()
tellView l x = tell $ setL (l . view) x mempty

listenView :: (MonadWriterView b m) => Lens b c -> m a -> m (a, c)
listenView l = liftM (id *** getL (l . view)) . listen

passView :: (MonadWriterView b m) => Lens b c -> m (a, c -> c) -> m a
passView l = pass . liftM (id *** modL (l . view))

censorView :: (MonadWriterView b m) => Lens b c -> (c -> c) -> m a -> m a
censorView l f = passView l . liftM (,f)

type MonadStateView b m = (MonadState' m, HasLens (MState m) b)

getView :: (MonadStateView b m) => Lens b c -> m c
getView l = liftM (getL $ l . view) get

putView :: (MonadStateView b m) => Lens b c -> c -> m ()
putView l = modify . setL (l . view)

modifyView  :: (MonadStateView b m) => Lens b c -> (c -> c) -> m ()
modifyView l = modify . modL (l . view)

type (MonadRWS' m) = (MonadReader' m, MonadWriter' m, MonadState' m)
type (MonadRWSView r o s m) =
  ( MonadRWS' m
  , HasLens (MEnv m) r
  , HasLens (MOut m) o
  , HasLens (MState m) s
  )

