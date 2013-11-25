module FP.Data.Stream where

import Prelude ()

data Step s a =
    Done
  | Next s a

data Stream m a = forall s. Stream 
  { step :: s -> m (Step s a)
  , seed :: s
  }
