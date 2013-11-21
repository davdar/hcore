{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Vis.Plot.Concrete where

import Prelude ()
import FP
import Vis.Plot.StateSpace
import Vis.Plot.Generic

newtype CPlotT m a = CPlotT { unCPlotT :: RWST PlotEnv PlotOut () m a }
  deriving (Monad, MonadTrans, MonadIO, MonadReader PlotEnv, MonadWriter PlotOut, MonadState ())

type instance MEnv (CPlotT m) = PlotEnv
type instance MOut (CPlotT m) = PlotOut
type instance MState (CPlotT m) = ()

runCPlotT :: (Monad m) => PlotEnv -> CPlotT m a -> m (a, PlotOut)
runCPlotT e t = evalRWST (unCPlotT t) e ()

execCPlotT :: (Monad m) => PlotEnv -> CPlotT m a -> m a
execCPlotT = liftM fst .: runCPlotT

mapCPlotT :: (Monad m) => (forall a. m a -> m a) -> CPlotT m a -> CPlotT m a
mapCPlotT f = CPlotT . mapRWST f . unCPlotT

type CPlot = CPlotT IO

runCPlot :: PlotEnv -> CPlot a -> IO (a, PlotOut)
runCPlot = runCPlotT

execCPlot :: PlotEnv -> CPlot a -> IO a
execCPlot = execCPlotT

plotit :: CPlot () -> IO ()
plotit = execCPlot defaultPlotEnv . display
