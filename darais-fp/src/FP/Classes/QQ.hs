module FP.Classes.QQ where

import Prelude ()
import FP.Data.Proxy
import Language.Haskell.TH.Quote

class QQ a where
  qq :: Proxy a -> QuasiQuoter
