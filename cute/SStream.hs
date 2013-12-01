mapM :: forall i m a b. (Monad m) => (a -> m b) -> SStreamT i m a -> SStreamT i m b
mapM f (SStreamT (seed0 :: s i) step) = SStreamT seed0 step'
  where
    step' :: forall i'. s i' -> m (Step s i' b)
    step' seed = do
      s <- step seed
      case s of
        Done -> return Done
        Yield a seed' -> do
          b <- f a
          return $ Yield b seed'

