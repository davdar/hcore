module FP.Classes.Sequence where

import Prelude ()

class (CFunctor t) => Sequence t where
  toStream :: (Compat t a) => t a -> Stream a
  fromStream :: (Compat t a) => Int -> Stream a -> t a
  (!&) :: (Compat t a) => t a -> Int -> a

class 
  ( Static1 t
  , CFunctor t
  , CFunctor (UnStatic1 t)
  , Compat t ~ Compat (UnStatic1 t)
  , Sequence (Static t)
  ) => SSequence t where
    toStreamS :: (Compat (t i) a) => t i a -> SStream i a
    fromStreamS :: (Compat (t i) a) => SInt i -> SStream i a -> t i a
    (!) :: (Compat t a) => t i a -> SInt i -> a

-- iterL :: (Iterable t) => (Elem t -> b -> b) -> b -> t -> b
-- iterL = flip . iterOnL
-- 
-- iterR :: (Iterable t) => (Elem t -> b -> b) -> b -> t -> b
-- iterR = flip . iterOnR
-- 
-- iterDoL :: (Iterable t) => t -> b -> (Elem t -> b -> b) -> b
-- iterDoL = flip . flip iterOnL
-- 
-- iterDoR :: (Iterable t) => t -> b -> (Elem t -> b -> b) -> b
-- iterDoR = flip . flip iterOnR
-- 
-- iterOnLM :: (Monad m, Iterable t) => (Elem t -> b -> m b) -> t -> b -> m b
-- iterOnLM f t = iterOnL ((=<<) . f) t . return
-- 
-- iterOnRM :: (Monad m, Iterable t) => (Elem t -> b -> m b) -> t -> b -> m b
-- iterOnRM f t = iterOnR ((=<<) . f) t . return
-- 
-- iterLM :: (Monad m, Iterable t) => (Elem t -> b -> m b) -> b -> t -> m b
-- iterLM = flip . iterOnLM
-- 
-- iterRM :: (Monad m, Iterable t) => (Elem t -> b -> m b) -> b -> t -> m b
-- iterRM = flip . iterOnRM
-- 
-- iterDoLM :: (Monad m, Iterable t) => t -> b -> (Elem t -> b -> m b) -> m b
-- iterDoLM = flip . flip iterOnLM
-- 
-- iterDoRM :: (Monad m, Iterable t) => t -> b -> (Elem t -> b -> m b) -> m b
-- iterDoRM = flip . flip iterOnRM
-- 
-- forLM :: (Monad m, Iterable t) => t -> (Elem t -> m ()) -> m ()
-- forLM t f = iterOnL (flip (>>) . f) t $ return ()
-- 
-- forRM :: (Monad m, Iterable t) => t -> (Elem t -> m ()) -> m ()
-- forRM t f = iterOnR (flip (>>) . f) t $ return ()
-- 
-- mapReduceDo :: (Iterable t) => t -> (b -> b -> b) -> b -> (Elem t -> b) -> b
-- mapReduceDo t r z f = iterL (r . f) z t
-- 
-- mapReduceDoM :: (Monad m, Iterable t) => t -> (b -> b -> b) -> b -> (Elem t -> m b) -> m b
-- mapReduceDoM t r z f = iterLM (\ e b -> liftM (r b) $ f e) z t
-- 
-- forBetweenLM :: (Monad m, Iterable t) => t -> m () -> (Elem t -> m ()) -> m ()
-- forBetweenLM t sep f = flip evalStateT True $ forLM t $ \ e -> do
--   isFirst <- get
--   put False
--   when (not isFirst) $ lift sep
--   lift $ f e
-- 
-- toList :: (Iterable t) => t -> [Elem t]
-- toList = iterR (:) []
-- 
-- infixl 9 !
-- class (Iterable t) => IdxIterable t where
--   type Index t :: *
--   iiterOnL :: (Index t -> Elem t -> b -> b) -> t -> b -> b
--   iiterOnR :: (Index t -> Elem t -> b -> b) -> t -> b -> b
--   (!) :: t -> Index t -> Elem t
-- 
-- iiterL :: (IdxIterable t) => (Index t -> Elem t -> b -> b) -> b -> t -> b
-- iiterL = flip . iiterOnL
-- 
-- iiterR :: (IdxIterable t) => (Index t -> Elem t -> b -> b) -> b -> t -> b
-- iiterR = flip . iiterOnR
-- 
-- iiterDoL :: (IdxIterable t) => t -> b -> (Index t -> Elem t -> b -> b) -> b
-- iiterDoL = flip . flip iiterOnL
-- 
-- iiterDoR :: (IdxIterable t) => t -> b -> (Index t -> Elem t -> b -> b) -> b
-- iiterDoR = flip . flip iiterOnR
-- 
-- iiterOnLM :: (Monad m, IdxIterable t) => (Index t -> Elem t -> b -> m b) -> t -> b -> m b
-- iiterOnLM f t = iiterOnL ((=<<) .: f) t . return
-- 
-- iiterOnRM :: (Monad m, IdxIterable t) => (Index t -> Elem t -> b -> m b) -> t -> b -> m b
-- iiterOnRM f t = iiterOnR ((=<<) .: f) t . return
-- 
-- iiterLM :: (Monad m, IdxIterable t) => (Index t -> Elem t -> b -> m b) -> b -> t -> m b
-- iiterLM = flip . iiterOnLM
-- 
-- iiterRM :: (Monad m, IdxIterable t) => (Index t -> Elem t -> b -> m b) -> b -> t -> m b
-- iiterRM = flip . iiterOnRM
-- 
-- iiterDoLM :: (Monad m, IdxIterable t) => t -> b -> (Index t -> Elem t -> b -> m b) -> m b
-- iiterDoLM = flip . flip iiterOnLM
-- 
-- iiterDoRM :: (Monad m, IdxIterable t) => t -> b -> (Index t -> Elem t -> b -> m b) -> m b
-- iiterDoRM = flip . flip iiterOnRM
-- 
-- iforLM :: (Monad m, IdxIterable t) => t -> (Index t -> Elem t -> m ()) -> m ()
-- iforLM t f = iiterOnL (flip (>>) .: f) t $ return ()
-- 
-- iforRM :: (Monad m, IdxIterable t) => t -> (Index t -> Elem t -> m ()) -> m ()
-- iforRM t f = iiterOnR (flip (>>) .: f) t $ return ()
-- 
-- iterOnLFromIiter :: (IdxIterable t) => (Elem t -> b -> b) -> t -> b -> b
-- iterOnLFromIiter f = iiterOnL (const f)
-- 
-- iterOnRFromIiter :: (IdxIterable t) => (Elem t -> b -> b) -> t -> b -> b
-- iterOnRFromIiter f = iiterOnR (const f)
-- 
-- itoList :: (IdxIterable t) => t -> [(Index t, Elem t)]
-- itoList = iiterR (curry (:)) []
