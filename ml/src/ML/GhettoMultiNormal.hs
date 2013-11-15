module ML.GhettoMultiNormal where

import ML.MonadGen
import Statistics.Distribution.Normal
import Data.Nat
import Data.V.Prim (V)
import qualified Data.V.Prim as V
import ML.Statistics

data GhettoMultiNormal i = GhettoMultiNormal
  { ghettoDim :: !(SInt i)
  , ghettoMean :: !(V i Double)
  , ghettoVariance :: !Double
  }

ghettoMultiNormal :: SInt i -> V i Double -> Double -> GhettoMultiNormal i
ghettoMultiNormal = GhettoMultiNormal

sampleGhettoMultiNormal :: (MonadGen m) => GhettoMultiNormal i -> m (V i Double)
sampleGhettoMultiNormal gmn =
  V.forM (ghettoMean gmn) $ \ mu ->
    genContVarM $ normalDistr mu $ ghettoVariance gmn
