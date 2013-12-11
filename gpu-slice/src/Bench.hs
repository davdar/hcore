module Main where

import Criterion
import Criterion.Environment
import Criterion.Config
import Criterion.Monad
import Model

main :: IO ()
main = do
  bs <- mapM benchDim [startDim..maxDim]
  withConfig config $ do
    env <- measureEnvironment
    runAndAnalyse (const True) env $ bgroup "" bs
  where
    config = defaultConfig
      { cfgReport = ljust "report.html"
      }
    startDim, maxDim :: Int
    startDim = 20
    maxDim = 20
    mhIters = 1
    benchDim d = do
      xs <- synthData (2 ^ d)
      return $ bgroup ("N=" ++ show (2 ^ d :: Int))
        [ bench "serial" $ nfIO $
            lsampleWith (ljointSerial xs) mhIters
        , bench "parallel" $ nfIO $
            lsampleWith (ljointPar xs) mhIters
        , bench "gpu" $ nfIO $
            lsampleWith (ljointAcc xs) mhIters
        -- , bench "gpu1" $ nfIO $
        --     lsampleWith (ljointAcc1 xs) mhIters
        -- , bench "gpu2" $ nfIO $
        --     lsampleWith (ljointAcc2 xs) mhIters
        ]
