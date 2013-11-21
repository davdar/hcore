module ML.Statistics where

import Prelude ()
import FP
import Statistics.Distribution
import ML.MonadGen

genContVarM :: (ContGen d, MonadGen m) => d -> m Double
genContVarM d = do
  g <- askGen
  liftIO $ genContVar d g
