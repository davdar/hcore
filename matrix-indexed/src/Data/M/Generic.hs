{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.M.Generic where

import Control.Monad.ST
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Prelude ()
import FP
import Data.Int.Indexed
import Data.V.Generic (IVector, UnIndexedV, IVectorComplete)
import qualified Data.V.Generic as V
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Generic.Mutable as MVector
import Data.Vector.Generic (Mutable)
import Text.Pretty.Generic as P
import Text.Pretty.StateSpace as P
import qualified Data.List as List
import Data.L (L)

class (IVector (FlatM t) a) => IMatrix (t::Nat -> Nat -> * -> *) a where
  type FlatM t :: Nat -> * -> *
  rollM :: SInt i -> SInt j -> FlatM t (i*j) a -> t i j a
  unrollM :: t i j a -> FlatM t (i*j) a
  rowsM :: t i j a -> SInt i
  colsM :: t i j a -> SInt j

type IMatrixComplete t i j a = 
  ( IdxIterable (t i j a), Elem (t i j a) ~ a, Index (t i j a) ~ (BInt i, BInt j)
  , IdxIterable (FlatM t i a), Elem (FlatM t i a) ~ a, Index (FlatM t i a) ~ BInt i
  , IdxIterable (FlatM t j a), Elem (FlatM t j a) ~ a, Index (FlatM t j a) ~ BInt j
  )

-- Helpers
fromFlatIndex :: SInt j -> BInt (i*j) -> (BInt i, BInt j)
fromFlatIndex jS kB = (unsafeI *** unsafeI) $ stripI kB `divMod` stripI jS

toFlatIndex :: SInt j -> (BInt i, BInt j) -> BInt (i*j)
toFlatIndex jS (iB, jB) = unsafeI $ stripI iB * stripI jS + stripI jB

-- Introduction
build :: (IMatrix t a) => SInt i -> SInt j -> ((BInt i, BInt j) -> a) -> t i j a
build iS jS f = rollM iS jS $ V.build (iS |*| jS) $ f . fromFlatIndex jS

fromLRows :: (IMatrix t a) => SInt i -> SInt j -> L (i*j) a -> t i j a
fromLRows iS jS = rollM iS jS . V.fromL

buildM :: (Monad m, IMatrix t a) => SInt i -> SInt j -> ((BInt i, BInt j) -> m a) -> m (t i j a)
buildM iS jS f = liftM (rollM iS jS) $ V.buildM (iS |*| jS) $ f . fromFlatIndex jS

fill :: (IMatrix t a) => SInt i -> SInt j -> a -> t i j a
fill iS jS = build iS jS . const

fillM :: (Monad m, IMatrix t a) => SInt i -> SInt j -> m a -> m (t i j a)
fillM iS jS = buildM iS jS . const

rowFromVector :: forall t i j a. (IMatrix t a) => FlatM t j a -> t 1 j a
rowFromVector v = withEqRefl (timesIdentityL :: j :=: (1 * j)) $ rollM (sint :: SInt 1) (V.length v) v

colFromVector :: forall t i j a. (IMatrix t a) => FlatM t i a -> t i 1 a
colFromVector v = withEqRefl (timesIdentityR :: i :=: (i * 1)) $ rollM (V.length v) (sint :: SInt 1) v

fromNested :: 
  ( IMatrix t a        , IMatrixComplete t i j a
  , IVector v1 (v2 j a), IVectorComplete v1 i (v2 j a)
  , IVector v2 a       , IVectorComplete v2 j a
  ) => SInt i -> SInt j -> v1 i (v2 j a) -> t i j a
fromNested iS jS vv = build iS jS $ \ (iB, jB) -> vv ! iB ! jB

diag :: (IMatrix t a, Num a) => SInt i -> a -> t i i a
diag iS x = build iS iS $ \ (iB, jB) ->
  if iB == jB then x else 0

-- Elimination
_iiterOnL :: (IMatrix t a) => ((BInt i, BInt j) -> a -> b -> b) -> t i j a -> b -> b
_iiterOnL f m = V._iiterOnL (f . fromFlatIndex (cols m)) $ unrollM m

_iiterOnR :: (IMatrix t a) => ((BInt i, BInt j) -> a -> b -> b) -> t i j a -> b -> b
_iiterOnR f m = V._iiterOnR (f . fromFlatIndex (cols m)) $ unrollM m

toListRows :: (IMatrix t a) => t i j a -> [[a]]
toListRows m =
  iterDoR (rows m) [] $ \ iB ->
    (:) $ {- (iB,) $ -} iterDoR (cols m) [] $ \ jB ->
      (:) $ {- (jB,) $ -} m `_index` (iB, jB)

colToVector :: forall t i a. (IMatrix t a) => t i 1 a -> FlatM t i a
colToVector m = withEqRefl (timesIdentityR :: i :=: (i * 1)) $ unrollM m

rowToVector :: forall t j a. (IMatrix t a) => t 1 j a -> FlatM t j a
rowToVector m  = withEqRefl (timesIdentityL :: j :=: (1 * j)) $ unrollM m

getDiag :: (IMatrix t a, IMatrixComplete t i i a) => t i i a -> FlatM t i a
getDiag m = V.build (rows m) $ \ iB -> m ! (iB, iB)

fromSingleton :: (IMatrix t a, IMatrixComplete t 1 1 a) => t 1 1 a -> a
fromSingleton m = m ! (unsafeI 0, unsafeI 0)

-- Mapping
_icmapM :: (Monad m, IMatrix t a, IMatrix t b) => ((BInt i, BInt j) -> a -> m b) -> t i j a -> m (t i j b)
_icmapM f m = liftM (rollM (rows m) (cols m)) $ V._icmapM (f . fromFlatIndex (cols m)) $ unrollM m

-- Attributes
rows :: (IMatrix t a) => t i j a -> SInt i
rows = rowsM

cols :: (IMatrix t a) => t i j a -> SInt j
cols = colsM

-- Indexing
_index :: (IMatrix t a) => t i j a -> (BInt i, BInt j) -> a
_index m = V._index (unrollM m) . toFlatIndex (cols m)

-- Combination
transpose :: (IMatrix t a) => t i j a -> t j i a
transpose m = build (cols m) (rows m) $ _index m . swap

colConcat :: (IMatrix t a) => t i j1 a -> t i j2 a -> t i (j1+j2) a
colConcat m1 m2 = 
  let rS = rows m1
      c1S = cols m1
      c1 = stripI c1S
      c2S = cols m2
      cnS = c1S |+| c2S
  in build rS cnS $ \ (iB, jB) ->
    let j = stripI jB
    in if j < c1
      then m1 `_index` (iB, unsafeI j)
      else m2 `_index` (iB, unsafeI $ j - c1)

infixl 6 %+%
(%+%) :: (Num a, IMatrix t a, IMatrixComplete t i j a) => t i j a -> t i j a -> t i j a
(%+%) m1 m2 = build (rows m1) (cols m1) $ \ idx -> m1 ! idx + m2 ! idx

infixl 6 %-%
(%-%) :: (Num a, IMatrix t a, IMatrixComplete t i j a) => t i j a -> t i j a -> t i j a
(%-%) m1 m2 = build (rows m1) (cols m1) $ \ idx -> m1 ! idx - m2 ! idx

infixl 7 %*%
(%*%) :: (Num a, IMatrix t a, IMatrixComplete t i j a, IMatrixComplete t j k a) => t i j a -> t j k a -> t i k a
(%*%) m1 m2 = build (rows m1) (cols m2) $ \ (iS, kS) -> getRow iS m1 V.%*% getCol kS m2

getRow :: (IMatrix t a, IMatrixComplete t i j a) => BInt i -> t i j a -> FlatM t j a
getRow iB m = V.build (cols m) $ \ jB -> m ! (iB, jB)

getCol :: (IMatrix t a, IMatrixComplete t i j a) => BInt j -> t i j a -> FlatM t i a
getCol jB m = V.build (rows m) $ \ iB -> m ! (iB, jB)

trans :: (IMatrix t a, IMatrixComplete t i j a) => t i j a -> t j i a
trans m = build (cols m) (rows m) $ (!) m . swap

-- Printing
_pretty :: forall m t i j a. (MonadPretty m, IMatrix t a) => PrettyF a -> t i j a -> m ()
_pretty prettyA m = do
  w <- askView P.layoutWidthL
  let dataRows :: forall m. (MonadPretty m) => [[m ()]]
      dataRows = map (map $ align . prettyA) $ toListRows m

      flatLength :: CPretty () -> Int
      flatLength = fromIntegral . T.length . execCPretty noConsolePrettyEnv . P.flat

      maxRowIndexWidth :: Int
      maxRowIndexWidth = List.length $ show $ stripI (rows m) - 1

      maxColIndexWidth :: Int
      maxColIndexWidth = List.length $ show $ stripI (cols m) - 1

      maxWidth :: Int
      maxWidth = fromIntegral $ maximum $ (:) maxColIndexWidth $ map (maximum . (:) 0 . map flatLength) dataRows

      colsPerLine :: Int
      colsPerLine =
        let widthPlusSep = maxWidth + 1
            rowHeaderWidthPlusSep = maxRowIndexWidth + 1
            colsThatFit = (w - rowHeaderWidthPlusSep) `div` widthPlusSep
            roomForHeadingCol = colsThatFit
        in max 0 roomForHeadingCol

      chopInto :: forall a. Int -> [a] -> [[a]]
      chopInto i l
        | List.length l <= i = [l]
        | otherwise = take i l : chopInto i (drop i l)

      -- example of transpose step in chopped
      -- [ [ (a, [(1, a1), (2, a2)])
      --   , (a, [(3, a3), (4, a4)])]
      -- , [ (b, [(1, b1), (2, b2)])
      --   , (b, [(3, b3), (4, b4)])]]
      -- =>
      -- [ [ (a, [(1, a1), (2, a2)])
      --   , (b, [(1, b1), (2, b2)])]
      -- , [ (a, [(3, a3), (4, a4)])
      --   , (b, [(3, b3), (4, b4)])]]

      -- returns list of ( ColIndicies            -- header for section
      --                 , [(RowIndex, [Value])]  -- data rows
      --                 )
      chopped :: [([Int], [(Int, [m ()])])]
      chopped = 
        let indexed :: [(Int, [(Int, m ())])]
            indexed = zip [0..] $ map (zip [0..]) dataRows
            diced :: [(Int, [[(Int, m ())]])]
            diced = map (second $ chopInto colsPerLine) indexed
            pushed :: [[(Int, [(Int, m ())])]]
            pushed = map propagate diced
            trans :: [[(Int, [(Int, m ())])]]
            trans = List.transpose pushed
            pulled :: [([Int], [(Int, [m ()])])]
            pulled = map unpropagate trans
        in pulled

      propagate :: forall a b c. (a, [[(b, c)]]) -> [(a, [(b, c)])]
      propagate (a, bcs) = map ((,) a) bcs

      unpropagate :: forall a b c. [(a, [(b, c)])] -> ([b], [(a, [c])])
      unpropagate abcs = 
        let as :: [a]
            bcss :: [[(b, c)]]
            (as, bcss) = unzip abcs
            bs :: [b]
            css :: [[c]]
            (bs, css) = first (fromMaybe [] . headM) $ unzip $ map unzip bcss
        in (bs, zip as css)
        where
          headM [] = Nothing
          headM (x:_) = Just x

      headerRow :: [Int] -> m ()
      headerRow = displayRow ":" . (:) (return ()) . map (P.punctuation . pretty)

      dataRow :: (Int, [m ()]) -> m ()
      dataRow (i, ds) = displayRow "|" $ (P.punctuation $ pretty i) : ds

      displayRow :: Text -> [m ()] -> m ()
      displayRow sep r = case r of
        [] -> return ()
        x:xs -> do
          flatFillToR maxRowIndexWidth x
          P.punctuation $ text ":"
          forBetweenLM xs (P.punctuation $ text sep) $ flatFillTo maxWidth

      displayDimensions :: m ()
      displayDimensions = P.punctuation $ do
        P.text "["
        pretty $ rows m
        P.text "x"
        pretty $ cols m
        P.text "]"
  displayDimensions
  P.hardLine
  forBetweenLM chopped (hardLine >> hardLine) $ \ (colIndicies, rows) ->
    forBetweenLM (headerRow colIndicies : map dataRow rows) hardLine id
