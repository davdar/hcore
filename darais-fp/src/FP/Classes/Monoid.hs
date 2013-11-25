module FP.Classes.Monoid where

import Prelude ()

appToEndo :: a -> Endo a -> a
appToEndo = flip appEndo

