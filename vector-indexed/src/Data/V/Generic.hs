{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.V.Generic where

import qualified Data.List as List
import Prelude ()
import FP hiding (length)
import Data.Int.Indexed
import Text.Pretty.Generic as P
import Data.L (L)
import qualified Data.L as L
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import Data.Vector.Fusion.Stream.Monadic (Stream)

class (Vector (UnIndexedV v) a) => IVector (v::Nat -> * -> *) a where
  type UnIndexedV v :: * -> *
  stripIV :: v i a -> UnIndexedV v a
  unsafeIV :: UnIndexedV v a -> v i a

-- Introduction
empty :: (IVector v a) => v 0 a
empty = unsafeIV Vector.empty

singleton :: (IVector v a) => a -> v 1 a
singleton = unsafeIV . Vector.singleton

fromL :: (IVector v a) => L i a -> v i a
fromL = unsafeIV . Vector.fromList . L.strip

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

-- Printing
_pretty :: (MonadPretty m, IVector v a, IdxIterable (v i a), Index (v i a) ~ BInt i, Elem (v i a) ~ a) => PrettyF a -> v i a -> m ()
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
