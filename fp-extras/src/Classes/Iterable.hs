{-# LANGUAGE TypeFamilies #-}

module Classes.Iterable where

import FPUtil.Function
import Control.Monad.State
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text

class Iterable t where
  type Elem t :: *
  iterOnL :: (Elem t -> b -> b) -> t -> b -> b
  iterOnR :: (Elem t -> b -> b) -> t -> b -> b

instance Iterable [a] where
  type Elem [a] = a
  iterOnL f xs b = List.foldl' (flip f) b xs
  iterOnR f xs b = Prelude.foldr f b xs

instance Iterable Text where
  type Elem Text = Char
  iterOnL f xs b = Text.foldl' (flip f) b xs
  iterOnR f xs b = Text.foldr f b xs

iterL :: (Iterable t) => (Elem t -> b -> b) -> b -> t -> b
iterL = flip . iterOnL

iterR :: (Iterable t) => (Elem t -> b -> b) -> b -> t -> b
iterR = flip . iterOnR

iterDoL :: (Iterable t) => t -> b -> (Elem t -> b -> b) -> b
iterDoL = flip . flip iterOnL

iterDoR :: (Iterable t) => t -> b -> (Elem t -> b -> b) -> b
iterDoR = flip . flip iterOnR

iterOnLM :: (Monad m, Iterable t) => (Elem t -> b -> m b) -> t -> b -> m b
iterOnLM f t = iterOnL ((=<<) . f) t . return

iterOnRM :: (Monad m, Iterable t) => (Elem t -> b -> m b) -> t -> b -> m b
iterOnRM f t = iterOnR ((=<<) . f) t . return

iterLM :: (Monad m, Iterable t) => (Elem t -> b -> m b) -> b -> t -> m b
iterLM = flip . iterOnLM

iterRM :: (Monad m, Iterable t) => (Elem t -> b -> m b) -> b -> t -> m b
iterRM = flip . iterOnRM

iterDoLM :: (Monad m, Iterable t) => t -> b -> (Elem t -> b -> m b) -> m b
iterDoLM = flip . flip iterOnLM

iterDoRM :: (Monad m, Iterable t) => t -> b -> (Elem t -> b -> m b) -> m b
iterDoRM = flip . flip iterOnRM

forLM :: (Monad m, Iterable t) => t -> (Elem t -> m ()) -> m ()
forLM t f = iterOnL (flip (>>) . f) t $ return ()

forRM :: (Monad m, Iterable t) => t -> (Elem t -> m ()) -> m ()
forRM t f = iterOnR (flip (>>) . f) t $ return ()

forBetweenLM :: (Monad m, Iterable t) => t -> m () -> (Elem t -> m ()) -> m ()
forBetweenLM t sep f = flip evalStateT True $ forLM t $ \ e -> do
  isFirst <- get
  put False
  when (not isFirst) $ lift sep
  lift $ f e

toList :: (Iterable t) => t -> [Elem t]
toList = iterR (:) []

class (Iterable t) => IdxIterable t where
  type Index t :: *
  iiterOnL :: (Index t -> Elem t -> b -> b) -> t -> b -> b
  iiterOnR :: (Index t -> Elem t -> b -> b) -> t -> b -> b
  (!) :: t -> Index t -> Elem t

iiterL :: (IdxIterable t) => (Index t -> Elem t -> b -> b) -> b -> t -> b
iiterL = flip . iiterOnL

iiterR :: (IdxIterable t) => (Index t -> Elem t -> b -> b) -> b -> t -> b
iiterR = flip . iiterOnR

iiterDoL :: (IdxIterable t) => t -> b -> (Index t -> Elem t -> b -> b) -> b
iiterDoL = flip . flip iiterOnL

iiterDoR :: (IdxIterable t) => t -> b -> (Index t -> Elem t -> b -> b) -> b
iiterDoR = flip . flip iiterOnR

iiterOnLM :: (Monad m, IdxIterable t) => (Index t -> Elem t -> b -> m b) -> t -> b -> m b
iiterOnLM f t = iiterOnL ((=<<) .: f) t . return

iiterOnRM :: (Monad m, IdxIterable t) => (Index t -> Elem t -> b -> m b) -> t -> b -> m b
iiterOnRM f t = iiterOnR ((=<<) .: f) t . return

iiterLM :: (Monad m, IdxIterable t) => (Index t -> Elem t -> b -> m b) -> b -> t -> m b
iiterLM = flip . iiterOnLM

iiterRM :: (Monad m, IdxIterable t) => (Index t -> Elem t -> b -> m b) -> b -> t -> m b
iiterRM = flip . iiterOnRM

iiterDoLM :: (Monad m, IdxIterable t) => t -> b -> (Index t -> Elem t -> b -> m b) -> m b
iiterDoLM = flip . flip iiterOnLM

iiterDoRM :: (Monad m, IdxIterable t) => t -> b -> (Index t -> Elem t -> b -> m b) -> m b
iiterDoRM = flip . flip iiterOnRM

iforLM :: (Monad m, IdxIterable t) => t -> (Index t -> Elem t -> m ()) -> m ()
iforLM t f = iiterOnL (flip (>>) .: f) t $ return ()

iforRM :: (Monad m, IdxIterable t) => t -> (Index t -> Elem t -> m ()) -> m ()
iforRM t f = iiterOnR (flip (>>) .: f) t $ return ()

iterOnLFromIiter :: (IdxIterable t) => (Elem t -> b -> b) -> t -> b -> b
iterOnLFromIiter f = iiterOnL (const f)

iterOnRFromIiter :: (IdxIterable t) => (Elem t -> b -> b) -> t -> b -> b
iterOnRFromIiter f = iiterOnR (const f)

itoList :: (IdxIterable t) => t -> [(Index t, Elem t)]
itoList = iiterR (curry (:)) []
