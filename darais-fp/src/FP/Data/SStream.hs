module FP.Data.SStream where

import Prelude ()
import qualified FP.Data.Stream as S
import FP.Data.Indexed
import FP.Data.Ex
import FP.Data.LibEq
import FP.Data.Stream(StreamT(..))
import FP.Classes.Static
import FP.PrePrelude
import FP.Classes.SNum
import FP.Data.Nat
import FP.Classes.Monad

data Step s (i::Nat) a where
  Done :: Step s Zero a
  Yield :: a -> s i -> Step s (Succ i) a

data SStreamT i m a = forall s. SStreamT
  { sstreamTSeed :: s i
  , sstreamTStep :: forall i'. s i' -> m (Step s i' a)
  }

type SStream i = SStreamT i Identity

sstream :: s i -> (forall i'. s i' -> Step s i' a) -> SStream i a
sstream seed step = SStreamT seed (return . step)

unsafeStep :: forall s i a. S.Step s a -> Step (Indexed s) i a
unsafeStep S.Done = withLibEq (unsafeLibEq :: i :=: 0) $
  Done
unsafeStep (S.Yield a s) = withLibEq (unsafeLibEq :: i :=: Succ 0) $ 
  Yield a (unsafeS s :: Indexed s 0)

stripStep :: forall s i a. Step s i a -> S.Step (Ex s) a
stripStep Done = S.Done
stripStep (Yield a s) = S.Yield a $ Ex s

unsafe :: forall i m a. (Monad m) => StreamT m a -> SStreamT i m a
unsafe (StreamT (seed0 :: s) step) = SStreamT seed0' step'
  where
    seed0' :: Indexed s i
    seed0' = unsafeS seed0
    step' :: forall i'. Indexed s i' -> m (Step (Indexed s) i' a)
    step' iS = liftM unsafeStep $ step $ stripS iS

strip :: forall i m a. (Monad m) => SStreamT i m a -> StreamT m a
strip (SStreamT (seed0 :: s i) step) = StreamT seed0' step'
  where
    seed0' :: Ex s
    seed0' = Ex seed0
    step' :: Ex s -> m (S.Step (Ex s) a)
    step' iEx = unEx iEx $ \ iS -> liftM stripStep $ step iS
