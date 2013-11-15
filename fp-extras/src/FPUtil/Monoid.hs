module FPUtil.Monoid where

import Data.Monoid

appToEndo :: a -> Endo a -> a
appToEndo = flip appEndo
