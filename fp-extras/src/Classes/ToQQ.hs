{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Classes.ToQQ where

import Control.Monad
import qualified Language.Haskell.TH as TH
import GHC.Exts

class ToQQ a where
  toQQE :: a -> TH.Q TH.Exp
  toQQP :: a -> TH.Q TH.Pat

instance ToQQ Integer where
  toQQE = TH.litE . TH.integerL
  toQQP = TH.litP . TH.integerL

instance ToQQ Double where
  toQQE d = TH.conE 'D# `TH.appE` TH.litE (TH.doublePrimL (toRational d))
  toQQP d = TH.conP 'D# [TH.litP (TH.doublePrimL (toRational d))]

instance ToQQ Char where
  toQQE = TH.litE . TH.charL
  toQQP = TH.litP . TH.charL

instance (ToQQ a) => ToQQ [a] where
  toQQE = liftM TH.ListE . mapM toQQE
  toQQP = liftM TH.ListP . mapM toQQP
