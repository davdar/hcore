{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Vis.Plot.StateSpace where

import Prelude ()
import FP
import Data.Lens.Template

data PlotEnv = PlotEnv
  { _titleL :: String
  , _xRangeL :: Maybe (Double, Double)
  , _yRangeL :: Maybe (Double, Double)
  , _resolutionL :: Int
  , _loggingL :: Bool
  , _cleanupL :: Bool
  , _outFileL :: String
  , _numBinsL :: Int
  } deriving (Eq, Ord, Show)
makeLens ''PlotEnv

defaultPlotEnv :: PlotEnv
defaultPlotEnv = PlotEnv
  { _titleL = ""
  , _xRangeL = Nothing
  , _yRangeL = Nothing
  , _resolutionL = 200
  , _loggingL = True
  , _cleanupL = True
  , _outFileL = "out.png"
  , _numBinsL = 20
  }

type Style = String

data PlotInfo = PlotInfo
  { plotFilename :: FilePath
  , plotTitle :: String
  , plotStyle :: Style
  }

data PlotOut = PlotOut
  { _plotInfoL :: [PlotInfo]
  }
makeLens ''PlotOut
instance Monoid PlotOut where
  mempty = PlotOut []
  mappend (PlotOut pi1) (PlotOut pi2) = PlotOut (pi1 <> pi2)

type MonadPlot m = (MonadIO m , MonadRWSView PlotEnv PlotOut () m)

