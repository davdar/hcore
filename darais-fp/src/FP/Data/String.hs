module FP.Data.String where

import Prelude ()
import Data.Char
import FP.PrePrelude
import FP.Classes.Functor
import FP.Data.List ()

slugify :: String -> String
slugify = map slugChar
  where
    slugChar c
      | isAlphaNum c = toLower c
      | otherwise = '_'

