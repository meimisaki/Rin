module Type.Kind
( checkKind
, inferKind
) where

import Text.PrettyPrint
import Type.PrettyPrint
import Type.Types
import Type.Unify

checkKind :: Type -> Kind -> TI ()
checkKind ty kind = inferKind ty >>= unify kind

inferKind :: Type -> TI Kind
inferKind (TyForall tvs rho) = do
  kvs <- mapM (const newTyVar) tvs
  foldr (uncurry extendKnEnv) cont (zip names kvs)
  where names = map getTyVarName tvs
        cont = do
          checkKind rho KnStar
          return KnStar
inferKind (TyArr ty1 ty2) = do
  checkKind ty1 KnStar
  checkKind ty2 KnStar
  return KnStar
inferKind (TyCon tc) = lookupKnEnv tc
inferKind (TyVar tv) = lookupKnEnv (getTyVarName tv)
inferKind (TyAp tau1 tau2) = do
  kind1 <- inferKind tau1
  (kind2, kind) <- unifyArr kind1
  checkKind tau2 kind2
  return kind
inferKind (TyMeta tv) = do
  mbTau <- readMeta tv
  case mbTau of
    Nothing -> throwTI $ hsep
      [ text "Can not infer kind for meta variable:"
      , pprint tv ]
    Just tau -> inferKind tau
