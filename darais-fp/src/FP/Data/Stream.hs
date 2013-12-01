module FP.Data.Stream where

import Prelude ()
import FP.PrePrelude
import FP.Data.Function
import FP.Classes.Monad

data Step s a =
    Done
  | Yield a s

data StreamT m a = forall s. StreamT
  { streamTSeed :: s
  , streamTStep :: s -> m (Step s a)
  }

takeStepM :: (Monad m) => StreamT m a -> m (Step (StreamT m a) a)
takeStepM (StreamT seed step) = do
  t <- step seed
  return $ case t of
    Done -> Done
    Yield a seed' -> Yield a $ StreamT seed' step

takeStep :: Stream a -> Step (Stream a) a
takeStep = runIdentity . takeStepM

untakeStep :: forall m a. (Monad m) => Step (StreamT m a) a -> StreamT m a
untakeStep step0 = StreamT seed0 step
  where
    seed0 :: Step (StreamT m a) a
    seed0 = step0
    step :: Step (StreamT m a) a -> m (Step (Step (StreamT m a) a) a)
    step Done = return Done
    step (Yield a (s :: StreamT m a)) = liftM (Yield a) $ takeStepM s

run :: forall m a. (Monad m) => StreamT m a -> m (Stream a)
run (StreamT (seed0 :: s) step) = loop seed0
  where
    loop :: s -> m (Stream a)
    loop seed = do
      s <- step seed
      case s of
        Done -> return $ untakeStep Done
        Yield (x :: a) (seed' :: s) -> do
          (xs :: Stream a) <- loop seed'
          return $ untakeStep $ Yield x xs

exec :: (Monad m) => StreamT m () -> m ()
exec (StreamT seed0 step) = loop seed0
  where
    loop seed = do
      s <- step seed
      case s of
        Done -> return ()
        Yield () seed' -> loop seed'

type Stream = StreamT Identity

stream :: s -> (s -> Step s a) -> Stream a
stream seed step = StreamT seed (return . step)

liftStream :: (Monad m) => Stream a -> StreamT m a
liftStream (StreamT seed0 step) = StreamT seed0 (return . runIdentity . step)

iterLM :: (Monad m) => (a -> b -> m b) -> b -> StreamT m a -> m b
iterLM f i0 (StreamT seed0 step) = loop i0 seed0
  where
    loop i seed = do
      s <- step seed
      case s of
        Done -> return i
        Yield a seed' -> do
          i' <- f a i
          seq i' $ loop i' seed'

iterDoLM :: (Monad m) => StreamT m a -> b -> (a -> b -> m b) -> m b
iterDoLM s i f = iterLM f i s

iterRM :: (Monad m) => (a -> b -> m b) -> b -> StreamT m a -> m b
iterRM f i0 (StreamT seed0 step) = loop seed0
  where
    loop seed = do
      s <- step seed
      case s of
        Done -> return i0
        Yield a seed' -> f a =<< loop seed'

iterDoRM :: (Monad m) => StreamT m a -> b -> (a -> b -> m b) -> m b
iterDoRM s i f = iterLM f i s

intersperse :: (Monad m) => a -> StreamT m a -> StreamT m a
intersperse inter (StreamT seed0 step) = StreamT (Nothing, seed0) step'
  where
    step' (Just a, seed) = return $ Yield a (Nothing, seed)
    step' (Nothing, seed) = do
      s <- step seed
      return $ case s of
        Done -> Done
        Yield a seed' -> Yield inter (Just a, seed')

mapM :: (Monad m) => (a -> m b) -> StreamT m a -> StreamT m b
mapM f (StreamT seed0 step) = StreamT seed0 step'
  where
    step' seed = do
      s <- step seed
      case s of
        Done -> return Done
        Yield a seed' -> do
          b <- f a
          return $ Yield b seed'

sequenceM :: (Monad m) => StreamT m (m a) -> StreamT m a
sequenceM = mapM id

map :: (Monad m) => (a -> b) -> StreamT m a -> StreamT m b
map f = mapM (return . f)

forIntersperseM :: (Monad m) => StreamT m a -> m b -> (a -> m b) -> StreamT m b
forIntersperseM s inter f = sequenceM $ intersperse inter $ map f s

sequenceIntersperseM :: (Monad m) => m a -> StreamT m (m a) -> StreamT m a
sequenceIntersperseM inter = sequenceM . intersperse inter

iterL :: (a -> b -> b) -> b -> Stream a -> b
iterL f = runIdentity .: iterLM (Identity .: f)

iterDoL :: Stream a -> b -> (a -> b -> b) -> b
iterDoL s i f = iterL f i s

iterR :: (a -> b -> b) -> b -> Stream a -> b
iterR f = runIdentity .: iterRM (Identity .: f)

iterDoR :: Stream a -> b -> (a -> b -> b) -> b
iterDoR s i f = iterR f i s
