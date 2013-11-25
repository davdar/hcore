module FP.Data.Proxy where

import Prelude ()

data Proxy (a::k) = Proxy

proxy :: Proxy a
proxy = Proxy
