{-# LANGUAGE TypeFamilies #-}

module Type.Unify
( Unifiable (..)
, unifyTyArr
, unifyKnArr
) where

import qualified Data.Set as S
import Text.PrettyPrint
import Type.Meta
import Type.PrettyPrint
import Type.Types

class (Meta a, MetaOf a ~ MetaVar a) => Unifiable a where
  unify :: a -> a -> TI ()
  unifyVar :: MetaOf a -> a -> TI ()
  unifyUnboundVar :: MetaOf a -> a -> TI ()
  {-# MINIMAL unify, unifyUnboundVar #-}
  unifyVar var1 obj2 = do
    mbObj1 <- readMeta var1
    case mbObj1 of
      Nothing -> unifyUnboundVar var1 obj2
      Just obj1 -> unify obj1 obj2

instance Unifiable Tau where
  unify (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return ()
  unify (TyVar tv1) (TyVar tv2)
    | isBound tv1 || isBound tv2 = throwTI $ hsep
      [ text "Unexpected types in unification:"
      , pprint tv1
      , text "and"
      , pprint tv2 ]
    | tv1 == tv2 = return ()
  unify (TyMeta tv1) (TyMeta tv2) | tv1 == tv2 = return ()
  unify (TyMeta tv) tau = unifyVar tv tau
  unify tau (TyMeta tv) = unifyVar tv tau
  unify (TyAp tau1 tau2) (TyAp tau1' tau2') = do
    unify tau1 tau1'
    unify tau2 tau2'
  unify tau1 tau2 = throwTI $ hsep
    [ text "Can not unify types:"
    , pprint tau1
    , text "vs."
    , pprint tau2 ]
  unifyUnboundVar tv1 (TyMeta tv2) = do
    mbTau2 <- readMeta tv2
    case mbTau2 of
      Nothing -> writeMeta tv1 (TyMeta tv2)
      Just tau2 -> unify (TyMeta tv1) tau2
  unifyUnboundVar tv1 tau2 = do
    tvs2 <- getMetaVars tau2
    if S.member tv1 tvs2
      then throwTI $ hsep
        [ text "Occurs-check failed for"
        , pprint tv1
        , text "in:"
        , pprint tau2 ]
      else writeMeta tv1 tau2

unifyTyArr :: Rho -> TI (Sigma, Rho)
unifyTyArr (TyArr sigma1 sigma2) = return (sigma1, sigmaToRho sigma2)
unifyTyArr (TyMono tau) = do
  tau1 <- newTyVar
  tau2 <- newTyVar
  unify tau (TyArr0 tau1 tau2)
  return (tauToSigma tau1, TyMono tau2)
-- TODO: optimize `TyMeta` case

instance Unifiable Kind where
  unify KnStar KnStar = return ()
  unify (KnMeta kv1) (KnMeta kv2) | kv1 == kv2 = return ()
  unify (KnMeta kv) kind = unifyVar kv kind
  unify kind (KnMeta kv) = unifyVar kv kind
  unify (KnArr kind1 kind2) (KnArr kind1' kind2') = do
    unify kind1 kind1'
    unify kind2 kind2'
  unify kind1 kind2 = throwTI $ hsep
    [ text "Can not unify kinds:"
    , pprint kind1
    , text "vs."
    , pprint kind2 ]
  unifyUnboundVar kv1 (KnMeta kv2) = do
    mbKind2 <- readMeta kv2
    case mbKind2 of
      Nothing -> writeMeta kv1 (KnMeta kv2)
      Just kind2 -> unify (KnMeta kv1) kind2
  unifyUnboundVar kv1 kind2 = do
    kvs2 <- getMetaVars kind2
    if S.member kv1 kvs2
      then throwTI $ hsep
        [ text "Kind occurs-check failed for"
        , pprint kv1
        , text "in:"
        , pprint kind2 ]
      else writeMeta kv1 kind2

unifyKnArr :: Kind -> TI (Kind, Kind)
unifyKnArr (KnArr kind1 kind2) = return (kind1, kind2)
unifyKnArr kind = do
  kind1 <- newKnVar
  kind2 <- newKnVar
  unify kind (KnArr kind1 kind2)
  return (kind1, kind2)
-- TODO: optimize `KnMeta` case
