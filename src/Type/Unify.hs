module Type.Unify
( unify
, unifyArr
) where

import qualified Data.Set as S
import Text.PrettyPrint
import Type.Meta
import Type.PrettyPrint
import Type.Types

unify :: Tau -> Tau -> TI ()
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
unify (TyArr tau1 tau2) (TyArr tau1' tau2') = do
  unify tau1 tau1'
  unify tau2 tau2'
unify (TyAp tau1 tau2) (TyAp tau1' tau2') = do
  unify tau1 tau1'
  unify tau2 tau2'
unify tau1 tau2 = throwTI $ hsep
  [ text "Can not unify types:"
  , pprint tau1
  , text "vs."
  , pprint tau2 ]

unifyVar :: TyMeta -> Tau -> TI ()
unifyVar var1 tau2 = do
  mbTau1 <- readMeta var1
  case mbTau1 of
    Nothing -> unifyUnboundVar var1 tau2
    Just tau1 -> unify tau1 tau2

unifyUnboundVar :: TyMeta -> Tau -> TI ()
unifyUnboundVar tv1 (TyMeta tv2) = do
  mbTau2 <- readMeta tv2
  case mbTau2 of
    Nothing -> writeMeta tv1 (TyMeta tv2)
    Just tau2 -> unify (TyMeta tv1) tau2
unifyUnboundVar tv1 tau2 = do
  tvs2 <- getMetaVars [tau2]
  if S.member tv1 tvs2
    then throwTI $ hsep
      [ text "Occurs-check failed for"
      , pprint tv1
      , text "in:"
      , pprint tau2 ]
    else writeMeta tv1 tau2

unifyArr :: Rho -> TI (Sigma, Rho)
unifyArr (TyArr sigma rho) = return (sigma, rho)
unifyArr tau = do
  tau1 <- newTyVar
  tau2 <- newTyVar
  unify tau (TyArr tau1 tau2)
  return (tau1, tau2)
-- TODO: optimize `TyMeta` case
