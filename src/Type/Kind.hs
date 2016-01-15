module Type.Kind
( Kinded (..)
) where

import Text.PrettyPrint
import Type.PrettyPrint
import Type.Types
import Type.Unify

class Kinded a where
  checkKind :: a -> Kind -> TI ()
  inferKind :: a -> TI Kind
  {-# MINIMAL inferKind #-}
  checkKind ty kind = inferKind ty >>= unify kind

instance Kinded Sigma where
  inferKind (TyForall tvs rho) = do
    kvs <- mapM (const newKnVar) tvs
    foldr (uncurry extendKnEnv) (inferKind rho) (zip names kvs)
    where names = map getTyVarName tvs

instance Kinded Rho where
  inferKind rho = do
    case rho of
      TyMono tau -> checkKind tau KnStar
      TyArr sigma1 sigma2 -> do
        checkKind sigma1 KnStar
        checkKind sigma2 KnStar
    return KnStar

instance Kinded Tau where
  inferKind (TyCon tc) = lookupKnEnv tc
  inferKind (TyVar tv) = lookupKnEnv (getTyVarName tv)
  inferKind (TyAp tau1 tau2) = do
    kind1 <- inferKind tau1
    (kind2, kind) <- unifyKnArr kind1
    checkKind tau2 kind2
    return kind
  inferKind (TyMeta tv) = do
    mbTau <- readMeta tv
    case mbTau of
      Nothing -> throwTI $ hsep
        [ text "Can not infer kind for meta variable:"
        , pprint tv ]
      Just tau -> inferKind tau
