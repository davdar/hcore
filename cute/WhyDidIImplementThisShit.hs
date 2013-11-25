
type LexicalSym i j = j <= i
data LexicalLt ix jx iy jy where
  LexicalLtI ::              ix < iy -> LexicalLt ix jx iy jy
  LexicalLtJ :: (ix ~ iy) => jx < jy -> LexicalLt ix jx iy jy

data LexicalSymB i j where
  LexicalSymB :: 
    forall i' j' i j. 
    (SInt i', SInt j')
    -> LexicalSym i' j' 
    -> LexicalLt i' j' i j
    -> LexicalSymB i j

-- TODO: make this call to the function for the whole matrix, not just the
-- diagonal (so no branching) and make cholesky fill in the zeros after its
-- branch
buildLowerTriDep :: 
  forall t i a. (IMatrix t a) 
  => SInt i
  -> a
  -> (forall i' j' m. (Monad m)
      => (LexicalSymB i' j' -> m a) 
      -> (SInt i', SInt j')
      -> (LexicalSym i' j', i' < i, j' < i)
      -> m a)
  -> t i i a
buildLowerTriDep iS zero f = rollM iS iS $ V.unsafeIV $ Vector.create vM
  where
    vM :: forall s. ST s (Mutable (UnIndexedV (FlatM t)) s a)
    vM = do
      v <- MVector.new $ stripI $ iS |*| iS
      let git :: forall i' j'. LexicalSymB i' j' -> ST s a
          git (LexicalSymB (i'S, j'S) _ _) = 
            MVector.read v $ 
            stripI $ 
            toFlatIndex iS (unsafeI $ stripI i'S, unsafeI $ stripI j'S)
      forLM iS $ \ (iB :: BInt i) ->
        forLM iS $ \ (jB :: BInt i) ->
        bintElim iB $ \ (i'S :: SInt i') (i'Lti :: i' < i) ->
        bintElim jB $ \ (j'S :: SInt j') (j'Lti :: j' < i) -> do
          let idxB = toFlatIndex iS (iB,jB)
          x <- case icompare i'S j'S of
            ILt (i'Ltj' :: i' < j') -> return zero
            IEq -> 
              f git (j'S, j'S) (lteRefl :: j' <= j', j'Lti, j'Lti)
            IGt (j'Lti' :: j' < i') -> 
              f git (i'S, j'S) (lteFromLt j'Lti' :: j' <= i', i'Lti, j'Lti)
          MVector.write v (stripI idxB) x
      return v

isSymmetric :: forall t i a. (IMatrix t a, IMatrixComplete t i i a) => (a -> a -> Bool) -> t i i a -> Bool
isSymmetric eqv m =
  iterDoL (rows m) True $ \ (iB :: BInt i) ->
  bintElim iB $ \ (jS :: SInt j) (jLti :: j < i) ->
  (&&) $ iterDoL jS True $ \ (jB :: BInt j) ->
    let jB' = bintExtend jB $ lteFromLt jLti
    in (&&) $ (m ! (iB, jB')) `eqv` (m ! (jB', iB))

cholesky :: forall t i a. (Floating a, IMatrix t a, IMatrixComplete t i i a) => (a -> a -> Bool) -> t i i a -> t i i a
cholesky eqv m 
  | not $ isSymmetric eqv m = error "matrix is not symmetric"
  | otherwise = buildLowerTriDep (rows m) 0 $ \ git  (i'S :: SInt i',j'S :: SInt j') (i'Symj', i'Lti, j'Lti) ->
      case icompare i'S j'S of
        ILt (i'Ltj' :: i' < j') -> ltAbsurdLte i'Ltj' i'Symj'
        IEq -> do
          rowSum <- mapReduceDoM j'S (+) 0 $ \ (j'B :: BInt j') ->
            bintElim j'B $ \ (kS :: SInt k) (kLtj' :: k < j') -> do
              x <- git $ LexicalSymB (j'S,kS) (lteFromLt kLtj'  :: k <= j') 
                                              (LexicalLtJ kLtj' :: LexicalLt j' k j' j')
              return $ x ^ 2
          return $ sqrt $ m ! (bint j'S j'Lti, bint j'S j'Lti) - rowSum
        IGt (j'Lti' :: j' < i') -> do
          rowSums <- mapReduceDoM j'S (+) 0 $ \ (j'B :: BInt j') ->
            bintElim j'B $ \ (kS :: SInt k) (kLtj' :: k < j') -> do
              x <- git $ LexicalSymB (i'S,kS) (lteFromLt $ ltTrans kLtj' j'Lti' :: k <= i') 
                                              (LexicalLtJ kLtj'                 :: LexicalLt i' k i' j')
              y <- git $ LexicalSymB (j'S,kS) (lteFromLt kLtj'                  :: k <= j')                  
                                              (LexicalLtI j'Lti'                :: LexicalLt j' k i' j')
              return $ x * y
          d <- git $ LexicalSymB (j'S,j'S) (lteRefl           :: j' <= j') 
                                           (LexicalLtI j'Lti' :: LexicalLt j' j' i' j')
          return $ 1 / d * (m ! (bint i'S i'Lti, bint j'S j'Lti) - rowSums)

-- upperTriangular :: (IMatrix t a) => t i i a -> Maybe (t i i a, Bool)
-- upperTriangular m = do
--   forLM cols $ \ j -> do
--     firstEachLM [j .. (rows - 1)] $ \ i -> do
--       when (read i j == 0) mzero
--       swapRow j i
--     diag <- read j j
--     forLM [(j+1) .. (rows - 1)] $ \ i -> do
--       when (read i j /= 0) $ do
--         headPos <- read i j
--         subMultRow (diag / headPos) j i
--         
-- 
--   eachFirst row $ r ->
--     when (m ! (r,c) == 0) fail
--     
--     swapRow c 0 r
--     negDet
--     each row past c $ r ->
--       when (m ! (r,c) /= 0) $
--         multRow r (m ! (r,c) * row c)
--         row c

