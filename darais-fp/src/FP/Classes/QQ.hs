module FP.Classes.QQ where

import Prelude ()

class QQ where
  qq :: Proxy a -> QuasiQuoter
