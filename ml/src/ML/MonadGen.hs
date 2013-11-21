module ML.MonadGen where

import Prelude ()
import FP
import System.Random.MWC
import Vis.Plot.Concrete

class (MonadIO m) => MonadGen m where
  askGen :: m GenIO
  localGen :: (GenIO -> GenIO) -> m a -> m a

instance (MonadGen m, Monoid o) => MonadGen (WriterT o m) where
  askGen = lift askGen
  localGen f = WriterT . localGen f . runWriterT

instance (MonadGen m) => MonadGen (CPlotT m) where
  askGen = lift askGen
  localGen f = mapCPlotT (localGen f)
