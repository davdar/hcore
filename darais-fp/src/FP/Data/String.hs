module FP.Data.String where

import Prelude ()
import Prelude (String)
import FP.Classes.Functor

slugify :: String -> String
slugify = map slugChar
  where
    slugChar c
      | isAlphaNum c = toLower c
      | otherwise = '_'

