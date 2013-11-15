module ML.MonadGen where

import Control.Monad.Trans
import Control.Monad.Writer
import System.Random.MWC

class (MonadIO m) => MonadGen m where
  askGen :: m GenIO
  localGen :: (GenIO -> GenIO) -> m a -> m a

instance (MonadGen m, Monoid o) => MonadGen (WriterT o m) where
  askGen = lift askGen
  localGen f = WriterT . localGen f . runWriterT
