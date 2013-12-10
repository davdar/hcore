module Types where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as A
import qualified Data.Array.Repa as R
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV

type RArrayA = R.Array A.A
type RVectorA = RArrayA R.DIM1
type RMatrixA = RArrayA R.DIM2

type RArrayU = R.Array R.U
type RVectorU = RArrayU R.DIM1
type RMatrixU = RArrayU R.DIM2

type AArray = A.Array
type AScalar = AArray A.DIM0
type AVector = AArray A.DIM1
type AMatrix = AArray A.DIM2

type VectorB = BV.Vector
type VectorU = UV.Vector
