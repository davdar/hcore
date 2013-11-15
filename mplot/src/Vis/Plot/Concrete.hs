{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Vis.Plot.Concrete where

import Prelude ()
import FP
import Vis.Plot.StateSpace
import Vis.Plot.Generic

newtype CPlot a = CPlot { unCPlot :: RWST PlotEnv PlotOut () IO a }
  deriving (Monad, MonadIO, MonadReader PlotEnv, MonadWriter PlotOut, MonadState ())

type instance MEnv CPlot = PlotEnv
type instance MOut CPlot = PlotOut
type instance MState CPlot = ()

runCPlot :: PlotEnv -> CPlot a -> IO (a, PlotOut)
runCPlot e t = evalRWST (unCPlot t) e ()

execCPlot :: PlotEnv -> CPlot a -> IO a
execCPlot = liftM fst .: runCPlot

plotit :: CPlot () -> IO ()
plotit = execCPlot defaultPlotEnv . display
