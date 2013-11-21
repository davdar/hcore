{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.V.Generic where

import Control.Monad.ST
import qualified Data.List as List
import Prelude ()
import FP
import Data.Int.Indexed
import Text.Pretty.Generic as P
import Data.L (L)
import qualified Data.L as L
import Data.Vector.Generic (Vector, Mutable)
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Generic.Mutable as MVector
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import Data.Vector.Fusion.Stream.Monadic (Stream)

class (Vector (UnIndexedV v) a) => IVector (v::Nat -> * -> *) a where
  type UnIndexedV v :: * -> *
  stripIV :: v i a -> UnIndexedV v a
  unsafeIV :: UnIndexedV v a -> v i a

type IVectorComplete v i a =
  (IdxIterable (v i a), Elem (v i a) ~ a, Index (v i a) ~ BInt i)

-- Introduction
empty :: (IVector v a) => v 0 a
empty = unsafeIV Vector.empty

singleton :: (IVector v a) => a -> v 1 a
singleton = unsafeIV . Vector.singleton

fromL :: (IVector v a) => L i a -> v i a
fromL = unsafeIV . Vector.fromList . L.stripL

build :: (IVector v a) => SInt i -> (BInt i -> a) -> v i a
build iS  f = unsafeIV $ Vector.generate (stripI iS) $ f . unsafeI

buildM :: (Monad m, IVector v a) => SInt i -> (BInt i -> m a) -> m (v i a)
buildM iS f = liftM unsafeIV $ Vector.generateM (stripI iS) $ f . unsafeI

fill :: (IVector v a) => SInt i -> a -> v i a
fill iS = unsafeIV . Vector.replicate (stripI iS)

fillM :: (Monad m, IVector v a) => SInt i -> m a -> m (v i a)
fillM iS = liftM unsafeIV . Vector.replicateM (stripI iS)

iterateN :: (IVector v a) => SInt i -> a -> (a -> a) -> v i a
iterateN iS x f = unsafeIV $ Vector.iterateN (stripI iS) f x

iterateNM :: (Monad m, IVector v a) => SInt i -> a -> (a -> m a) -> m (v i a)
iterateNM iS x f = unstreamM $ MStream.iterateNM (stripI iS) f x

grid :: (Fractional a, IVector v a) => SInt i -> a -> a -> v i a
grid iS low high =
  build iS $ \ iB ->
    low + (fromIntegral $ stripI iB) * width
  where
    range = high - low
    width = range / (fromIntegral (stripI iS) - 1)

buildDep :: 
  forall v i a. (IVector v a) 
  => SInt i 
  -> (forall j m. (BInt j -> m a) -> SInt j -> j < i -> m a) 
  -> v i a
buildDep iS f = unsafeIV $ Vector.create vM
  where
    vM :: forall s. ST s (Mutable (UnIndexedV v) s a)
    vM = do
      v <- MVector.new $ stripI iS
      let git :: forall j. BInt j -> ST s a
          git = MVector.read v . stripI
      forLM iS $ \ (iB :: BInt i) -> do
        bintElim iB $ \ (jS :: SInt j) (jLti :: j < i) -> do
          MVector.write v (stripI iB) =<< f git jS jLti
      return v
    
-- Elimination
_iiterOnL :: (IVector v a) => (BInt i -> a -> b -> b) -> v i a -> b -> b
_iiterOnL f v b = Vector.ifoldl' (\ b' i a -> f (unsafeI i) a b') b $ stripIV v

_iiterOnR :: (IVector v a) => (BInt i -> a -> b -> b) -> v i a -> b -> b
_iiterOnR f v b = Vector.ifoldr' (f . unsafeI) b $ stripIV v

-- Mapping
_icmapM :: (IVector v a, IVector v b, Monad m) => (BInt i -> a -> m b) -> v i a -> m (v i b)
_icmapM f = unstreamM . Stream.mapM (uncurry $ f . unsafeI) . Stream.indexed . Vector.stream . stripIV

-- Attributes
length :: (IVector v a) => v i a -> SInt i
length = unsafeI . Vector.length . stripIV

-- Indexing
_index :: (IVector v a) => v i a -> BInt i -> a
_index v iS = stripIV v Vector.! stripI iS

-- Update
updateIndex :: (IVector v a) => BInt i -> a ->  v i a -> v i a
updateIndex iB x v = unsafeIV $ stripIV v Vector.// [(stripI iB, x)]

-- Combination
concat :: (IVector v a) => v i a -> v j a -> v (i+j) a
concat v1 v2 = unsafeIV $ stripIV v1 Vector.++ stripIV v2

infixl 6 %+%
(%+%) :: (IVector v a, Num a, IVectorComplete v i a) => v i a -> v i a -> v i a
(%+%) xs ys = build (length xs) $ \ iB -> (xs ! iB) + (ys ! iB)

infixl 6 %-%
(%-%) :: (IVector v a, Num a, IVectorComplete v i a) => v i a -> v i a -> v i a
(%-%) xs ys = build (length xs) $ \ iB -> (xs ! iB) - (ys ! iB)

infixl 7 %*%
(%*%) :: (IVector v a, Num a, IVectorComplete v i a) => v i a -> v i a -> a
(%*%) v1 v2 = iterDoL (length v1) 0 $ \ iB -> (+) $ (v1 ! iB) * (v2 ! iB)

infixl 7 *%
(*%) :: (IVector v a, CFunctor (v i), Compat (v i) a, Num a) => a -> v i a -> v i a
(*%) = cmap . (*)

infixl 7 %*
(%*) :: (IVector v a, CFunctor (v i), Compat (v i) a, Num a) => v i a -> a -> v i a
(%*) = flip (*%)

-- Printing
_pretty :: (MonadPretty m, IVector v a, IVectorComplete v i a) => PrettyF a -> v i a -> m ()
_pretty prettyDropIndentA v =
  let numWidth = List.length $ show' $ stripI (length v) - 1
      process iB a = group $ do
        P.punctuation $ do
          P.flatFillToR numWidth $ pretty $ stripI iB
          P.text ":" 
        prettyDropIndentA a
  in do
    P.punctuation $ do
      P.text "["
      pretty $ length v
      P.text "]"
    P.hardLine
    sequence_ $ List.intersperse hardLine $ map (uncurry process) $ itoList v

-- Internal
unstreamM :: (IVector v a, Monad m) => Stream m a -> m (v i a)
unstreamM s = do
  xs <- MStream.toList s
  return $ unsafeIV $ Vector.unstream $ MStream.unsafeFromList (MStream.size s) xs
