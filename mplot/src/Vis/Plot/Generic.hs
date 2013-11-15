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
import Data.M.Prim (M)
import Data.V.Prim (V)
import FP
import Prelude ()
import System.Cmd
import System.Directory
import System.IO
import Vis.Plot.StateSpace
import qualified Data.ByteString.Char8 as BS
import qualified Data.M.Generic as M
import qualified Data.V.Generic as V
import qualified Data.V.Prim as VP
import qualified Data.V.Boxed as VB
import qualified Data.List as List

tellPlotInfo :: (MonadPlot m) => PlotInfo -> m ()
tellPlotInfo = tellView plotInfoL . (:[])

noCleanup :: (MonadPlot m) => m a -> m a
noCleanup = local $ modL (cleanupL . view) $ const False

xRange :: (MonadPlot m) => (Double, Double) -> m a -> m a
xRange r = local $ modL (xRangeL . view) $ const $ Just r

plotData :: (MonadPlot m) => Style -> M i j Double -> m ()
plotData style m = do
  title <- askView titleL
  (tmpn,tmph) <- liftIO $ openTempFile "." $ slugify title ++ ".dat"
  liftIO $ do
    iforLM m $ \ (_, jB) e -> do
      when (stripI jB /= 0) $ BS.hPut tmph " "
      BS.hPut tmph $ BS.pack $ show e
      when (stripI jB == stripI (M.cols m) - 1) $ BS.hPut tmph "\n"
    hClose tmph
  tellPlotInfo $ PlotInfo tmpn title style

plotLines :: (MonadPlot m) => V i Double -> V i Double -> m ()
plotLines xs ys = plotData "lines" $ 
  M.colConcat (M.colVector xs) (M.colVector ys)

getXs :: (MonadPlot m) => m (ExI V Double)
getXs = do
  rSE <- liftM sintEx $ askView resolutionL
  Just (lb,ub) <- askView xRangeL
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
      vv = VB.build binsS $ \ binB ->
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
          VP.singleton center `V.concat` VP.singleton freq
  in M.fromNested binsS sint vv

-- hist2 :: forall i. V i Double -> M i 2 Double
-- hist2 v =
--   let numData = fromIntegral $ stripI $ V.length v :: Double
--   in withEqRefl (unsafeEqRefl :: (1+1) :=: 2) $
--     M.colConcat (M.colVector v) (M.fill (V.length v) (sint :: SInt 1) (1 / numData))

plotHist :: (MonadPlot m) => V i Double -> m ()
plotHist v = do
  numBinsEx <- liftM sintEx $ askView numBinsL
  unEx numBinsEx $ \ numBinsS ->
    plotData "boxes" $ inferHist numBinsS v

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
          flip map infos $ \ (PlotInfo file title style) -> 
            printf "'%s' title '%s' with %s" file title style
        cmd :: String
        cmd = 
          printf 
            (concat
              [ "gnuplot -persist -e \""
              , "set term pngcairo enhanced ;"
              , "set output '"
              , outFile
              , "' ;plot [%s] [%s] %s\""
              ])
            (maybe "" gpRange xrM) (maybe "" gpRange yrM) 
            cmdArgs
    when logging $ do
      BS.putStr "GNUPLOT:"
      BS.putStrLn $ BS.pack cmd
      BS.putStrLn "<<--PLOT-LOG"
    void $ system cmd

    when cleanup $
      forM_ infos $ \ (PlotInfo file _ _) ->
        removeFile file
  where
    gpRange (l,u) = show l ++ ":" ++ show u
