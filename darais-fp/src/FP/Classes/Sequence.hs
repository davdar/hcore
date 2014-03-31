module FP.Classes.Sequence where

import Prelude ()
import FP.PrePrelude
import FP.Data.Proxy
import FP.Data.Function
import FP.Classes.Monad
import qualified FP.Data.Stream as S
import FP.Data.Stream (StreamT(..), Stream)
import FP.Classes.Compat
import Control.Monad.Cont

class Sequence t where
  toStream :: (Compat t a) => t a -> Stream a
  fromStream :: (Compat t a) => Int -> Stream a -> t a
  length :: (Compat t a) => t a -> Int
  (!) :: (Compat t a) => t a -> Int -> a

type instance Compat Stream = Universal
instance Sequence Stream where
  toStream = id
  fromStream = const id
  length = slength
  (!) = sindexUnsafe

-- Direct Conversion

fromStreamP :: (Sequence t, Compat t a) => Proxy (t a) -> Int -> Stream a -> t a
fromStreamP = const fromStream

toStreamM :: (Monad m, Sequence t, Compat t a) => t a -> StreamT m a
toStreamM = S.liftStream . toStream

fromStreamM :: (Monad m, Sequence t, Compat t a) => Int -> StreamT m a -> m (t a)
fromStreamM i = liftM (fromStream i) . S.run

fromStreamMP :: (Monad m, Sequence t, Compat t a) => Proxy (t a) -> Int -> StreamT m a -> m (t a)
fromStreamMP = const fromStreamM

-- "Primitive" ops which wrap around StreamT ops

smapM :: (Monad m, Sequence t, Compat t a, Compat t b) => (a -> m b) -> t a -> m (t b)
smapM f xs = fromStreamM (length xs) $ S.mapM f $ toStreamM xs

sintersperseMapM :: (Monad m, Sequence t, Compat t a, Compat t b) => (a -> m b) -> m b -> t a -> m (t b)
sintersperseMapM f sep xs = fromStreamM (length xs) $ S.intersperseM sep $ smap f $ toStream xs

siterM :: (Monad m, Sequence t, Compat t a) => (a -> b -> m b) -> b -> t a -> m b
siterM f i = S.iterM f i . toStreamM

-- Length and indexing

slength :: (Sequence t, Compat t a) => t a -> Int
slength = flip execState 0 . straverse (const $ modify (+1))

sindexM :: (MonadPlus m, Sequence t, Compat t a) => t a -> Int -> m a
sindexM xs i = flip evalStateT 0 $ straverseSumOn xs $ \ x -> do
  xi <- get
  if i == xi
    then return x
    else mzero

sindexUnsafe :: (Sequence t, Compat t a) => t a -> Int -> a
sindexUnsafe = fromJust .: sindexM

---- Derived Variants

-- mapping

smap :: (Sequence t, Compat t a, Compat t b) => (a -> b) -> t a -> t b
smap f = runIdentity . smapM (return . f)

smapOn :: (Sequence t, Compat t a, Compat t b) => t a -> (a -> b) -> t b
smapOn = flip smap

smapOnM :: (Monad m, Sequence t, Compat t a, Compat t b) => t a -> (a -> m b) -> m (t b)
smapOnM = flip smapM

ssequence :: (Monad m, Sequence t, Compat t (m a), Compat t a) => t (m a) -> m (t a)
ssequence = smapM id

sintersperseMap :: (Sequence t, Compat t a, Compat t b) => (a -> b) -> b -> t a -> t b
sintersperseMap f sep = runIdentity . sintersperseMapM (return . f) (return sep)

sintersperseMapOn :: (Sequence t, Compat t a, Compat t b) => t a -> b -> (a -> b) -> t b
sintersperseMapOn xs sep f = sintersperseMap f sep xs

sintersperseMapOnM :: (Monad m, Sequence t, Compat t a, Compat t b) => t a -> m b -> (a -> m b) -> m (t b)
sintersperseMapOnM xs sep f = sintersperseMapM f sep xs

-- iteration

siter :: (Sequence t, Compat t a) => (a -> b -> b) -> b -> t a -> b
siter f = runIdentity .: siterM (return .: f)

siterFrom :: (Sequence t, Compat t a) => b -> (a -> b -> b) -> t a -> b
siterFrom = flip siter

siterOn :: (Sequence t, Compat t a) => t a -> b -> (a -> b -> b) -> b
siterOn xs i f = siter f i xs

siterOnM :: (Monad m, Sequence t, Compat t a) => t a -> b -> (a -> b -> m b) -> m b
siterOnM xs bM f = siterM f bM xs

siterRevM :: (Monad m, Sequence t, Compat t a) => (a -> b -> m b) -> b -> t a -> m b
siterRevM f = flip runContT return .: siterM (\ x i' -> ContT $ \ k -> f x =<< k i')

siterRev :: (Sequence t, Compat t a) => (a -> b -> b) -> b -> t a -> b
siterRev f = runIdentity .: siterRevM (return .: f)

siterRevOn :: (Sequence t, Compat t a) => t a -> b -> (a -> b -> b) -> b
siterRevOn xs i f = siterRev f i xs

siterRevOnM :: (Monad m, Sequence t, Compat t a) => t a -> b -> (a -> b -> m b) -> m b
siterRevOnM xs bM f = siterM f bM xs

straverse :: (Monad m, Sequence t, Compat t a) => (a -> m ()) -> t a -> m ()
straverse f = siter ((>>) . f) (return ())

straverseOn :: (Monad m, Sequence t, Compat t a) => t a -> (a -> m ()) -> m ()
straverseOn = flip straverse

sintersperseTraverse :: (Monad m, Sequence t, Compat t a) => (a -> m ()) -> m () -> t a -> m ()
sintersperseTraverse f sep = S.exec . S.intersperseM sep . smap f . toStream

sintersperseTraverseOn :: (Monad m, Sequence t, Compat t a) => t a -> m () -> (a -> m ()) -> m ()
sintersperseTraverseOn xs sep f = sintersperseTraverse f sep xs

straverseSum :: (MonadPlus m, Sequence t, Compat t a) => (a -> m b) -> t a -> m b
straverseSum f = siter (mplus . f) mzero

straverseSumOn :: (MonadPlus m, Sequence t, Compat t a) => t a -> (a -> m b) -> m b
straverseSumOn = flip straverseSum

sexecute :: (Monad m, Sequence t, Compat t (m ())) => t (m ()) -> m ()
sexecute = straverse id

sintersperseExecute :: (Monad m, Sequence t, Compat t (m ())) => m () -> t (m ()) -> m ()
sintersperseExecute = sintersperseTraverse id
