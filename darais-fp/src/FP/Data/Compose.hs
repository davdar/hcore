module FP.Data.Compose where

import Prelude ()

data (:.:) c d a = Compose { runCompose :: c (d a) }
