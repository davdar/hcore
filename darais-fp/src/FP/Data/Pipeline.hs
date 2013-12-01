module FP.Data.Pipeline where

import Prelude ()
import qualified Data.List as List
import FP.Data.Proxy
import FP.PrePrelude
import FP.Data.LibEq
import FP.Data.Peano
import FP.Classes.SNum

-- class GSequence shape t where
--   toStream :: (Compat t a) => t a -> GStream shape a
--   fromStream :: (Compat t a) => Dim shape -> GStream shape a -> t a
--   length :: (Compat t a) => t a -> Dim shape
--   (!) :: (Compat t a) => t a -> Dim shape -> a
-- 
-- class GGSequence t where
--   toStream :: (Compat t a) => t shape a -> GStream shape a
--   fromStream :: (Compat t a) => Dim shape -> GStream shape a -> t shape a
--   reduceS :: (a -> a -> a) -> a -> t (S shape) a -> t shape a
--   reduceP :: (Monad m) => (a -> a -> a) -> a -> t (S shape) a -> m (t shape a)
--   length :: (Compat t a) => t shape a -> Dim shape
--   (!) :: (Compat t a) => t shape a -> Dim shape -> a

data Shape =
    Arr Peano
  | Tup Shape Shape

-- data SShape (s::Shape) where
--   SArr :: (IPeano i) => SShape (Arr i)
--   STup :: (IShape s1, IShape s2) => SShape (Tup s1 s2)
-- 
-- class IShape (s::Shape) where
--   iShape :: SShape s
-- 
-- instance (IPeano n) => IShape (Arr n) where
--   iShape = SArr
-- 
-- instance (IShape s1, IShape s2) => IShape (Tup s1 s2) where
--   iShape = STup

data Exp var t where
  LitE :: t -> Exp var t
  VarE :: var t -> Exp var t
  PlusE :: Exp var Double -> Exp var Double -> Exp var Double
  PairE :: Exp var t -> Exp var u -> Exp var (t,u)
  AppE :: Arr var t u -> Exp var t -> Exp var u

data Arr var t u where
  ExpE :: Exp var t -> Arr var () t
  AbsA :: (var t -> Arr var u v) -> Arr var (t,u) v

data a :-> b where
  Id :: a :-> a 
  (:.:) :: (b :-> c) -> (a :-> b) -> (a :-> c)
  Const :: b -> (a :-> b)
  Dup :: a :-> (a, a)
  (:||:) :: (a1 :-> b1) -> (a2 :-> b2) -> (a1, a2) :-> (b1, b2)
  Plus :: (Double, Double) :-> Double
  Fst :: (a, b) :-> a
  Snd :: (a, b) :-> b

-- mapVarA :: (a :-> b) -> (b :-> a) -> Arr ((:->) a) c d -> Arr ((:->) b) c d
-- mapVarA (to :: a :-> b) (from :: b :-> a) (AbsA (g :: (a :-> c) -> Exp ((:->) a) d)) = result
--   where
--     result :: Arr ((:->) b) c d
--     result = AbsA abs
--     abs :: (b :-> c) -> Exp ((:->) b) d
--     abs h = mapVarE from $ g (h :.: to)
-- 
-- mapVarE :: a :-> b -> Exp ((:->) b) c -> Exp ((:->) a) c
-- mapVarE _ (LitE v) = LitE v
-- mapVarE f (VarE x) = VarE (x :.: f)
-- mapVarE f (PlusE e1 e2) = PlusE (mapVarE f e1) (mapVarE f e2)
-- mapVarE (f :: a :-> b) (AppE (abs :: Arr ((:->) b) c d) (e :: Exp ((:->) b) c)) = result
--   where
--     result :: Exp ((:->) a) d
--     result = AppE (undefined :: Arr ((:->) a) c d) (mapVarE f e)

compileA :: Arr ((:->) e) t u -> (e :-> t) -> (e :-> u)
compileA (ExpE e) _ = compileE e
compileA (AbsA f) t = compileA (f $ Fst :.: t) $ Snd :.: t

compileE :: Exp ((:->) t) u -> t :-> u
compileE (LitE v) = Const v
compileE (VarE x) = x
compileE (PlusE e1 e2) = Plus :.: (compileE e1 :||: compileE e2) :.: Dup
compileE (PairE e1 e2) = compileE e1 :||: compileE e2 :.: Dup
compileE (AppE abs e) = compileA abs $ compileE e

-- data Pipeline (s1::Shape) a (shape2::Shape) b where
--   Id :: Pipeline s a s a
--   Pipe :: Pipeline s1 a s2 b -> Pipeline s2 b s3 c -> Pipeline s1 a s3 c
--   Copy :: Pipeline s a (Tup s s) (a,a)
--   Reduce :: ((a,b) :-> b) -> b -> Pipeline (Arr (S n)) a (Arr n) b
--   Map :: (a :-> b) -> Pipeline (Arr n) a (Arr n) b
--   Parallel :: Pipeline s1 a s2 b -> Pipeline s1' a' s2' b' -> Pipeline (Tup s1 s1') (a,a') (Tup s2 s2') (b,b')
--   Outer :: ((a,b) :-> c) -> Pipeline (Tup (Arr n) (Arr m)) (a,b) (Arr (n ^+^ m)) c
-- 
-- data MDList (n::Peano) a where
--   MDZeroList :: a -> MDList Z a
--   MDSuccList :: MDList n [a] -> MDList (S n) a
-- 
-- instance Functor (MDList n) where
--   fmap f (MDZeroList x) = MDZeroList $ f x
--   fmap f (MDSuccList xs) = MDSuccList $ fmap (fmap f) xs
-- 
-- data ShapedList (s :: Shape) (a :: *) where
--   ShapedArr :: MDList n a -> ShapedList (Arr n) a
--   ShapedTup :: ShapedList s1 a -> ShapedList s2 b -> ShapedList (Tup s1 s2) (a, b)
-- 
-- reduceAll :: forall n a. (IPeano n) => ((a,a) :-> a) -> a -> Pipeline (Arr n) a (Arr Z) a
-- reduceAll f i = case iPeano :: SPeano n of
--   SZ -> Map (Curry f i)
--   SS _ -> Reduce f i `Pipe` reduceAll f i
-- 
-- apply  :: a :-> b -> a -> b
-- apply (f :.: g) = apply f . apply g
-- apply (Curry f x) = curry (apply f) x
-- 
-- interp :: forall s1 a s2 b. Pipeline s1 a s2 b -> ShapedList s1 a -> ShapedList s2 b
-- interp Id s = s
-- interp (Pipe p1 p2) xsS = interp p2 $ interp p1 xsS
-- interp Copy xsS = ShapedTup xsS xsS
-- interp (Reduce f i) (ShapedArr (MDSuccList (xs :: MDList n [a]))) = ShapedArr $ fmap (List.foldl' (flip $ curry $ apply f) i) xs
-- interp (Reduce _ _) _ = error "impossible"
-- interp (Map f) (ShapedArr (xs :: MDList n a)) = ShapedArr $ fmap (apply f) xs
-- interp (Map _) _ = error "impossible"
-- interp (Parallel p1 p2) (ShapedTup xs1S xs2S) = ShapedTup (interp p1 xs1S) (interp p2 xs2S)
-- interp (Parallel _ _) _ = error "impossible"
-- interp (Outer f) (ShapedTup (ShapedArr (xs1 :: MDList n a')) (ShapedArr (xs2 :: MDList m b'))) = ShapedArr $ outer (curry $ apply f) xs1 xs2
-- interp (Outer _) _ = error "impossible"
-- 
-- outer :: forall a n b m c. (a -> b -> c) -> MDList n a -> MDList m b -> MDList (n ^+^ m) c
-- outer f (xs1 :: MDList n a) (xs2 :: MDList m b) = case xs1 of
--   MDZeroList x -> fmap (f x) xs2
--   MDSuccList (xs :: MDList nPred [a]) ->
--     let f' :: [a] -> [b] -> [c]
--         f' = List.zipWith f
--         xs2' :: MDList m [b]
--         xs2' = fmap repeat xs2
--         r :: MDList (nPred ^+^ m) [c]
--         r = outer f' xs xs2'
--     in withLibEq (axiom1 (proxy :: Proxy n) (proxy :: Proxy nPred) (proxy :: Proxy m)) $ MDSuccList r
--   where
--     axiom1 :: forall i i' j. (i ~ S i') => Proxy i -> Proxy i' -> Proxy j -> S (i' ^+^ j) :=: i ^+^ j
--     axiom1 _ _ _ = unsafeLibEq
