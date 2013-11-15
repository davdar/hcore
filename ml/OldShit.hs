{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Control.Applicative
import Control.Monad
import System.IO
import ML.Statistics
import ML.MonadGen
import Classes.Accessible
import Data.Nat
import Data.M.Prim (M)
import Data.V.Prim (V)
import ML.Gen
import ML.GhettoMultiNormal
import ML.MetropolisHastings
import Statistics.Distribution
import Statistics.Distribution.Normal
import Text.Printf
import Vis.CPlot
import qualified Data.M.Prim as M
import qualified Data.V.Prim as V
import qualified Data.V.Boxed as V.B
import qualified Data.Vector.Primitive as Vector
import qualified Data.Vector as Vector.B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

testPlot :: IO ()
testPlot = exCPlot defaultPlotEnv $ xRange (-10,10) $ display $ do
  plotFun "sin" sin
  plotFun "x^2" (\ x -> x*x)

testHist :: IO ()
testHist =  exCPlot defaultPlotEnv $ display $ do
  V.ex (Vector.fromList $ replicate 12 1 ++ replicate 14 2 ++ replicate 11 3 ++ replicate 4 4 ++ [5,6]) $ \ v ->
    plotHist "hist" v

pdf :: V Ten Double -> Double
pdf theta =
  let vp :: Double
      vp = density (normalDistr 0 9) $ theta ! unsafeBInt 0
      xps :: Double
      xps = V.product $ V.forWithIndex theta $ \ iB e ->
        if stripBInt iB == 0
          then 1.0
          else density (normalDistr 0 (exp vp)) e
  in vp * xps

proposal :: (MonadGen m) => V Ten Double -> m (V Ten Double)
proposal x = sampleGhettoMultiNormal $ ghettoMultiNormal (unsafeSInt 10) x 1

initial :: V Ten Double
initial = V.replicate (unsafeSInt 10) 0

countLessThan :: Double -> V i Double -> Int
countLessThan y = V.foldl (\ r x -> r + if x < y then 1 else 0) 0

report :: M i Ten Double -> IO ()
report samples = do
  let marginal = M.fromCol samples (unsafeBInt 0) $ V.generate $ M.rows samples
  putStrLn $ printf "Less than -5==%s" $ show $ countLessThan (-5) marginal
  plotit $ plotHist "" marginal


discreteSamples :: (MonadGen m) => SInt i -> m (M i Ten Double)
discreteSamples iS = do
  vv <- V.B.replicateM iS $ do
    v <- genContVarM $ normalDistr 0 9
    xs <- V.replicateM (unsafeSInt 9) $ genContVarM $ normalDistr 0 (exp v)
    return $ V.concat (V.singleton v) xs
  return $ M.generate iS (unsafeSInt 10) $ \ (iB, jB) -> vv ! iB ! jB

plotDiscrete :: IO ()
plotDiscrete = exSInt 50000 $ \ iS -> do
  samples <- execMGen $ discreteSamples iS
  report samples

mhSamples :: (MonadGen m) => SInt i -> m (M i Ten Double, Int)
mhSamples = mh pdf proposal initial

plotMh :: IO ()
plotMh = exSInt 50000 $ \ iS -> do
  (samples, success) <- execMGen $ mhSamples iS
  putStrLn $ printf "SUCCESS==%s" $ show success
  report samples

sliceSamples :: (MonadGen m) => SInt i -> m (M i Ten Double)
sliceSamples = sliceMh pdf initial

plotSlice :: IO ()
plotSlice = exSInt 50000 $ \ iS -> do
  samples <- execMGen $ sliceSamples iS
  report samples

readJokes :: IO (Vector.B.Vector ByteString)
readJokes = do
  liftM Vector.B.fromList $ withFile "store/jester_items.clean.dat" ReadMode $ process
  where
    process h = do
      end <- hIsEOF h
      if end
        then return []
        else do
          void $ BS.hGetLine h
          void $ BS.hGetLine h
          pure (:) <*> BS.hGetLine h  <*> process h

processJokes :: forall i a. V.B.V i ByteString -> (forall j. (V.B.V j ByteString, M i j Int) -> a) -> a
processJokes jokes k =
  let jokes' :: V.B.V i [ByteString]
      jokes' = V.B.map BS.words jokes
      wordVEx :: Vector.B.Vector ByteString
      wordVEx = Vector.B.fromList $ Set.toList $ V.B.foldl (foldl' (flip Set.insert)) Set.empty jokes'
  in V.B.ex wordVEx $ \ (wordV :: V.B.V j ByteString) ->
  let idxMap :: Map ByteString (BInt j)
      idxMap = Map.fromList $ V.B.toList $ V.B.forWithIndex wordV $ flip (,)
      vv :: V.B.V i (V j Int)
      vv = V.B.for jokes' $ \ jws ->
        (\ f -> foldl' f (V.replicate (V.B.length wordV) 0) jws) $ \ v jw ->
          let idx = idxMap Map.! jw
          in V.updateIndex v idx (1 + v ! idx)
  in k (wordV, M.generate (V.B.length jokes) (V.B.length wordV) $ \ (iB, jB) -> vv ! iB ! jB)
  
-- i documents
-- k topics
-- j global words
-- n words in a given document
-- ldaPdf :: (MonadGen m)
--   => V i Int               -- ^documentLengths
--   -> Double                -- ^documentSeed    (alpha)
--   -> V i V (k Double)      -- ^documentTopics  (theta)
--   -> V i (Vector (BInt k)) -- ^wordTopics      (z)
--   -> Double                -- ^topicSeed       (eta)
--   -> V k (V j Double)      -- ^topicWords      (beta)
--   -> m Double
-- ldaPdf documentLengths documentSeed documentTopics wordTopics topicSeed topicWords =
--   liftM V.product $ V.forWithIndexM topicWords $ \ kB topicWord ->
--     pTopicWord <- dirichlet topicSeed topicWord
--     liftM ((*pTopicWord) . V.product) $ V.forWithIndexM documentTopics $ \ dB documentTopic ->
--       pDocumentTopic <- dirichlet topicSeed documentTopic
--       V.ex (wordTopics ! dB) $ \ wordTopics_d ->
--         liftM ((*pDocumentTopic) . V.product) $ V.forWithIndexM wordTopics_d $ \ nB wordTopic ->
--           pWordTopic <- categorical documentTopic wordTopic
--           pWord <- categorical topicWord (documents ! nB)
--           return $ pWordTopic * pWord

main :: IO ()
main = plotMh

