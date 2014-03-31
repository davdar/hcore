module FP.Data.Stream where

import Prelude ()
import FP.PrePrelude
import FP.Classes.Monad

data Step s a =
    Done
  | Yield a s

data StreamT m a = forall s. StreamT
  { streamTSeed :: s
  , streamTStep :: s -> m (Step s a)
  }
type Stream = StreamT Identity

-- Introduction and elimination

stream :: s -> (s -> Step s a) -> Stream a
stream seed step = StreamT seed (return . step)

liftStream :: (Monad m) => Stream a -> StreamT m a
liftStream (StreamT seed0 step) = StreamT seed0 (return . runIdentity . step)

unpackM :: (Monad m) => StreamT m a -> m (Step (StreamT m a) a)
unpackM (StreamT seed step) = do
  t <- step seed
  return $ case t of
    Done -> Done
    Yield a seed' -> Yield a $ StreamT seed' step

packM :: forall m a. (Monad m) => Step (StreamT m a) a -> StreamT m a
packM step0 = StreamT seed0 step
  where
    seed0 :: Step (StreamT m a) a
    seed0 = step0
    step :: Step (StreamT m a) a -> m (Step (Step (StreamT m a) a) a)
    step Done = return Done
    step (Yield a (s :: StreamT m a)) = liftM (Yield a) $ unpackM s

run :: forall m a. (Monad m) => StreamT m a -> m (Stream a)
run (StreamT (seed0 :: s) step) = loop seed0
  where
    loop :: s -> m (Stream a)
    loop seed = do
      s <- step seed
      case s of
        Done -> return $ packM Done
        Yield (x :: a) (seed' :: s) -> do
          (xs :: Stream a) <- loop seed'
          return $ packM $ Yield x xs

exec :: (Monad m) => StreamT m () -> m ()
exec (StreamT seed0 step) = loop seed0
  where
    loop seed = do
      s <- step seed
      case s of
        Done -> return ()
        Yield () seed' -> loop seed'

iterM :: (Monad m) => (a -> b -> m b) -> b -> StreamT m a -> m b
iterM f i0 (StreamT seed0 step) = loop i0 seed0
  where
    loop i seed = do
      s <- step seed
      case s of
        Done -> return i
        Yield a seed' -> do
          i' <- f a i
          seq i' $ loop i' seed'

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

intersperseM :: (Monad m) => m a -> Stream (m a) -> StreamT m a
intersperseM inter (StreamT seed0 step) = StreamT (Nothing, seed0) step'
  where
    -- the very first step
    step' (Nothing, seed) = do
      case runIdentity $ step seed of
        Done -> return Done
        Yield aM seed' -> do
          a <- aM
          return $ Yield a (Just Nothing, seed')
    -- in-between steps
    step' (Just Nothing, seed) = do
      case runIdentity $ step seed of
        Done -> return Done
        Yield aM seed' -> do
          i <- inter
          return $ Yield i (Just (Just aM), seed')
    step' (Just (Just aM), seed) = do
      a <- aM
      return $ Yield a (Just Nothing, seed)
