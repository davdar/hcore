module Main where

import ML.Bullshit

import Unsafe.Coerce
import Text.CSV
import Control.Exception.Base
import Data.Int.Indexed
import Data.M.Storable (M)
import Data.V.Storable (V, Storable)
import FP
import Statistics.Distribution.Uniform
import Statistics.Distribution.Gamma
import ML.Gen
import ML.MonadGen
import ML.Statistics
import Prelude ()
import Statistics.Distribution.Normal
import System.IO.Unsafe
import Vis.Plot
import qualified Data.List as List
import qualified Data.L as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.M.Generic as M hiding (trans)
import qualified Data.M.Storable as M
import qualified Data.V.Generic as V hiding (fromL, iterateN, fill, fillM, singleton, build, empty, buildM)
import qualified Data.V.Generic as VG
import qualified Data.V.Storable as V
import qualified Data.V.Boxed as VB
import qualified Data.Vector.Storable as Vector

main = main'
