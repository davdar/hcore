{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Vis.Plot.Generic where

import Debug.Trace
import Data.Int.Indexed
import Data.M.Boxed (M)
import Data.V.Boxed (V)
import FP
import Prelude ()
import System.Cmd
import System.Directory
import System.IO
import Vis.Plot.StateSpace
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.M.Generic as M
import qualified Data.V.Generic as V hiding (build, singleton)
import qualified Data.V.Boxed as V
import qualified Data.List as List

tellPlotInfo :: (MonadPlot m) => PlotInfo -> m ()
tellPlotInfo = tellView plotInfoL . (:[])

noCleanup :: (MonadPlot m) => m a -> m a
noCleanup = localViewSet cleanupL False

xRange :: (MonadPlot m) => (Maybe Double, Maybe Double) -> m a -> m a
xRange r = localViewSet xRangeL r

plotData :: (MonadPlot m) => Style -> String -> Options -> M i j ByteString -> m ()
plotData style using options m = do
  title <- askView titleL
  (tmpn,tmph) <- liftIO $ openTempFile "." $ slugify title ++ ".dat"
  liftIO $ do
    iforLM m $ \ (_, jB) e -> do
      when (stripI jB /= 0) $ BS.hPut tmph " "
      BS.hPut tmph e
      when (stripI jB == stripI (M.cols m) - 1) $ BS.hPut tmph "\n"
    hClose tmph
  tellPlotInfo $ PlotInfo tmpn title using style options

plotLines :: (MonadPlot m) => V i Double -> V i Double -> m ()
plotLines xs ys = plotData "lines" "" [] $ 
  cmap (BS.pack . show) $ M.colFromVector xs `M.colConcat` M.colFromVector ys

plotPoints :: (MonadPlot m) => V i Double -> V i Double -> m ()
plotPoints xs ys = do
  size <- askView pointSizeL
  shape  <- askView pointShapeL
  let options = [["pointsize", show size], ["pointtype", show $ pointShapeCode shape]]
  plotData "points" "" options $
    cmap (BS.pack . show) $ M.colFromVector xs `M.colConcat` M.colFromVector ys

plotLinesError :: (MonadPlot m) => V i Double -> V i Double -> V i Double -> V i Double -> m ()
plotLinesError xs ys ysLow ysHigh = do
  plotData "filledcurves" "" [["linecolor", "rgb '#bbbbbb'"]] $
    cmap (BS.pack . show) $ 
      M.colFromVector xs 
      `M.colConcat` M.colFromVector ysLow 
      `M.colConcat` M.colFromVector ysHigh
  plotLines xs ys

-- TODO: do something sensible if xRangeL isn't set??
getXs :: (MonadPlot m) => m (ExI V Double)
getXs = do
  rSE <- liftM sintEx $ askView resolutionL
  (Just lb, Just ub) <- askView xRangeL
  return $ unEx rSE $ \ rS ->
    let 
      pointWidth :: Double
      pointWidth = (ub - lb) / fromIntegral (stripEx rSE)
      scale :: Int -> Double
      scale i = lb + fromIntegral i * pointWidth
    in ExI $ V.build rS (scale . stripI)

plotFun :: (MonadPlot m) => (Double -> Double) -> m ()
plotFun f = do
  xsE <- getXs
  unExI xsE $ \ xs -> 
    plotLines xs $ cmap f xs

inferHist :: SInt i -> V j Double -> M i 2 Double
inferHist binsS v =
  if stripI (V.length v) == 0 
    then M.fill binsS sint 0
    else
      let bins = fromIntegral $ stripI binsS
          (low, high) = iterDoL v (1e10,-1e10) $ \ x (l,h) -> (min x l,  max x h)
          binWidth = (high - low) / (bins - 1)
      in trace (show [low, high, binWidth]) $ makeHist binsS binWidth low v

makeHist :: SInt i -> Double -> Double -> V j Double -> M i 2 Double
makeHist binsS binWidth minVal v =
  let halfBinWidth = binWidth / 2
      numData = fromIntegral $ stripI $ V.length v
      vv = V.build binsS $ \ binB ->
        let bin = fromIntegral $ stripI binB
            bucketMin = minVal + (bin - 1) * binWidth + halfBinWidth
            bucketMax = bucketMin + binWidth
            center :: Double
            center = bucketMin + halfBinWidth
            freq :: Double
            freq = iterDoL v 0.0 $ \ x r ->
              if x >= bucketMin && x < bucketMax
                then r + 1.0 / numData
                else r
        in withEqRefl (unsafeEqRefl :: (1+1) :=: 2) $ 
          V.singleton center `V.concat` V.singleton freq
  in M.fromNested binsS sint vv

plotHist :: (MonadPlot m) => V i Double -> m ()
plotHist v = do
  numBinsEx <- liftM sintEx $ askView numBinsL
  unEx numBinsEx $ \ numBinsS ->
    plotData "boxes" "" [] $ cmap (BS.pack . show) $ inferHist numBinsS v

labeledHist :: (MonadPlot m) => V i ByteString -> V i Double -> m ()
labeledHist labels values = do
  plotData "boxes" "using 0:2:xticlabels(1)" [] $
    M.colFromVector labels `M.colConcat` (cmap (BS.pack . show) $ M.colFromVector $ V.fromV values)

display :: (MonadPlot m) => m () -> m ()
display t = censor (const mempty) $ do
  (PlotOut infos) <- liftM (getL view . snd) $ listen t
  xrM <- askView xRangeL
  yrM <- askView yRangeL
  logging <- askView loggingL
  cleanup <- askView cleanupL
  outFile <- askView outFileL
  liftIO $ do
    when logging $ do
      BS.putStrLn "-->>PLOT-LOG"
      BS.putStrLn $ BS.pack $ printf "cleanup=%s" $ show cleanup
    let cmdArgs :: String
        cmdArgs = 
          List.intercalate "," $
          flip map infos $ \ (PlotInfo file title using style options) -> 
            let optionsS = unwords $ map unwords options
            in printf "'%s' %s title '%s' with %s %s" file using title style optionsS
        cmd :: String
        cmd = 
          printf 
            (concat
              [ "gnuplot -persist -e \""
              , "set term pngcairo enhanced ;"
              , "set xtics rotate by -45;"
              , "set output '"
              , outFile
              , ".png"
              , "' ;plot [%s] [%s] %s\""
              ])
            (gpRange xrM) (gpRange yrM) 
            cmdArgs
    when logging $ do
      BS.putStr "GNUPLOT:"
      BS.putStrLn $ BS.pack cmd
      BS.putStrLn "<<--PLOT-LOG"
    void $ system cmd

    when cleanup $
      forM_ infos $ \ info ->
        removeFile $ plotFilename info
  where
    gpRange (lM,uM) = maybe "" show lM ++ ":" ++ maybe "" show uM
