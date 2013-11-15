module FPUtil.String where

import Data.Char

slugify :: String -> String
slugify = map slugChar
  where
    slugChar c
      | isAlphaNum c = toLower c
      | otherwise = '_'
