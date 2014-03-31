module FP.Data.Lens
  ( module FP.Data.Lens
  , module Data.Lens.Common
  , module Data.Lens.Template
  ) where

import Prelude ()
import FP.PrePrelude
import Data.Lens.Common
import Data.Lens.Template
import FP.Classes.Monad

class HasLens a b where
  view :: Lens a b

instance HasLens a a where
  view = id
instance HasLens (a,b) a where
  view = fstLens
instance HasLens (a,b) b where
  view = sndLens

type MonadReaderView r m = (MonadReader' m, HasLens (MEnv m) r)

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

type MonadStateView s m = (MonadState' m, HasLens (MState m) s)

getView :: (MonadStateView b m) => Lens b c -> m c
getView l = liftM (getL $ l . view) get

putView :: (MonadStateView b m) => Lens b c -> c -> m ()
putView l = modify . setL (l . view)

modifyView  :: (MonadStateView b m) => Lens b c -> (c -> c) -> m ()
modifyView l = modify . modL (l . view)

type (MonadRWSView r o s m) = (MonadReaderView r m, MonadWriterView o m, MonadStateView s m)
