
axiom1 :: forall (i::Nat) (j::Nat). (i ~ (j + i)) => Proxy i -> Proxy j -> j :=: 0
axiom1 _ _ = unsafeLibEq

axiom2 :: forall (i::Nat) (j::Nat) (ij::Nat). (ij ~ (i + j)) => Proxy i -> Proxy j -> j < ij -> 0 < i
axiom2 _ _ _ = unsafeLtAxiom

axiom3 :: forall i b. 0 < i -> (forall i'. (i ~ Succ i') => Proxy i' -> b) -> b
axiom3 _ f = withLibEq (unsafeLibEq :: i :=: Succ 0) $ f (proxy :: Proxy 0)

axiom4 :: forall (i::Nat) (j::Nat) (ij::Nat). (ij ~ (Succ i + j)) => Proxy i -> Proxy j -> ij :=: (i + Succ j)
axiom4 _ _ = unsafeLibEq

absurd1 :: forall (i::Nat) (j::Nat) (ji::Nat) a. (ji ~ (i + j)) => Proxy i -> Proxy j -> ji < j -> a
absurd1 _ _ _ = error "absurd"

streamRowFromIndex :: forall t i jN a. (SSequence2D t, Compat (t i jN) a) => BInt i -> t i jN a -> SStream jN a
streamRowFromIndex iB t = sstream seed0 step
  where
    memoColsS :: SInt jN
    memoColsS = colsS t
    seed0 :: Sub jN jN
    seed0 = withLibEq (plusIdentityR :: jN :=: (jN + 0)) $ Sub (sint :: SInt 0)
    step :: forall j'. Sub jN j' -> S.Step (Sub jN) j' a
    step (Sub (kS :: SInt k)) = case compareS kS memoColsS of
      LtS (pf :: k < jN) ->
        let _0Ltj' :: 0 < j'
            _0Ltj' = axiom2 (proxy :: Proxy j') (proxy :: Proxy k) pf
        in 
        axiom3 _0Ltj' $ \ (_ :: Proxy j'') ->
        withLibEq (axiom4 (proxy :: Proxy j'') (proxy :: Proxy k)) $
        S.Yield (t |!!| (iB, bint kS pf)) (Sub $ succS kS :: Sub jN j'')
      EqS -> 
        withLibEq (axiom1 (proxy :: Proxy jN) (proxy :: Proxy j')) $
        withLibEq (plusIdentityR :: jN :=: (jN + 0)) $
        S.Done
      GtS (pf :: jN < k) -> absurd1 (proxy :: Proxy j') (proxy :: Proxy k) pf

