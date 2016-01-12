module Type.Unify
( unify
, unifyArr
) where

import qualified Data.Set as S
import Text.PrettyPrint
import Type.Class
import Type.PrettyPrint
import Type.Types

unify :: Tau -> Tau -> TI ()
unify (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return ()
unify (TyVar tv1) (TyVar tv2)
  | isBound tv1 || isBound tv2 = throwError (show err)
  | tv1 == tv2 = return ()
  where err = hsep
          [ text "Unexpected types in unification:"
          , pprint tv1
          , text "and"
          , pprint tv2 ]
unify (TyMeta tv1) (TyMeta tv2) | tv1 == tv2 = return ()
unify (TyMeta tv) tau = unifyVar tv tau
unify tau (TyMeta tv) = unifyVar tv tau
unify (TyArr0 tau1 tau2) (TyArr0 tau1' tau2') = do
  unify tau1 tau1'
  unify tau2 tau2'
unify tau1 tau2 = throwError (show err)
  where err = hsep
          [ text "Can not unify types:"
          , pprint tau1
          , text "vs."
          , pprint tau2 ]

unifyVar :: TyMeta -> Tau -> TI ()
unifyVar tv1 tau2 = do
  mbTau1 <- readTyMeta tv1
  case mbTau1 of
    Nothing -> unifyUnboundVar tv1 tau2
    Just tau1 -> unify tau1 tau2

unifyUnboundVar :: TyMeta -> Tau -> TI ()
unifyUnboundVar tv1 (TyMeta tv2) = do
  mbTau2 <- readTyMeta tv2
  case mbTau2 of
    Nothing -> writeTyMeta tv1 (TyMeta tv2)
    Just tau2 -> unify (TyMeta tv1) tau2
unifyUnboundVar tv1 tau2 = do
  tvs2 <- getMetaTyVars tau2
  if S.member tv1 tvs2
    then throwError (show err)
    else writeTyMeta tv1 tau2
  where err = hsep
          [ text "Occurs-check failed for"
          , pprint tv1
          , text "in:"
          , pprint tau2 ]

unifyArr :: Rho -> TI (Sigma, Rho)
unifyArr (TyArr sigma1 sigma2) = return (sigma1, sigmaToRho sigma2)
unifyArr (TyMono tau) = do
  tau1 <- newTyVar
  tau2 <- newTyVar
  unify tau (TyArr0 tau1 tau2)
  return (tauToSigma tau1, TyMono tau2)
