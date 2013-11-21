{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Vis.Plot.StateSpace where

import Prelude ()
import FP
import Data.Lens.Template

data PointShape =
    OpenSquare
  | SolidSquare
  | OpenCircle
  | SolidCircle
  | OpenTriangle
  | SolidTriangle
  | Ascii Char
  deriving (Eq, Ord, Show)

pointShapeCode :: PointShape -> String
pointShapeCode OpenSquare = "4"
pointShapeCode SolidSquare = "5"
pointShapeCode OpenCircle = "6"
pointShapeCode SolidCircle = "7"
pointShapeCode OpenTriangle = "8"
pointShapeCode SolidTriangle = "9"
pointShapeCode (Ascii x) = '\'':x:'\'':""

data PlotEnv = PlotEnv
  { _titleL :: String
  , _xRangeL :: Maybe (Double, Double)
  , _yRangeL :: Maybe (Double, Double)
  , _resolutionL :: Int
  , _loggingL :: Bool
  , _cleanupL :: Bool
  , _outFileL :: String
  , _numBinsL :: Int
  , _pointSizeL :: Int
  , _pointShapeL :: PointShape
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
  , _outFileL = "out"
  , _numBinsL = 20
  , _pointSizeL = 2
  , _pointShapeL = OpenCircle
  }

type Style = String
type Options = [[String]]

data PlotInfo = PlotInfo
  { plotFilename :: FilePath
  , plotTitle :: String
  , plotStyle :: Style
  , plotOptions :: [[String]]
  }

data PlotOut = PlotOut
  { _plotInfoL :: [PlotInfo]
  }
makeLens ''PlotOut
instance Monoid PlotOut where
  mempty = PlotOut []
  mappend (PlotOut pi1) (PlotOut pi2) = PlotOut (pi1 <> pi2)

type MonadPlot m = (MonadIO m , MonadRWSView PlotEnv PlotOut () m)

