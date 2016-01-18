module Type.Meta
( zonk
, metaVars
, getMetaVars
) where

import qualified Data.Set as S
import Type.Types

zonk :: Type -> TI Type
zonk (TyForall tvs rho) = do
  rho' <- zonk rho
  return (mkForall tvs rho')
zonk (TyArr sigma1 sigma2) = do
  sigma1' <- zonk sigma1
  sigma2' <- zonk sigma2
  return (TyArr sigma1' sigma2')
zonk tau@(TyCon _) = return tau
zonk tau@(TyVar _) = return tau
zonk (TyAp tau1 tau2) = do
  tau1' <- zonk tau1
  tau2' <- zonk tau2
  return (TyAp tau1' tau2')
zonk tau@(TyMeta tv) = do
  mbTau1 <- readMeta tv
  case mbTau1 of
    Nothing -> return tau
    Just tau1 -> do
      tau2 <- zonk tau1
      writeMeta tv tau2
      return tau2

metaVars :: Type -> S.Set TyMeta
metaVars (TyForall _ rho) = metaVars rho
metaVars (TyArr sigma1 sigma2) = S.union tvs1 tvs2
  where tvs1 = metaVars sigma1
        tvs2 = metaVars sigma2
metaVars tau = go tau S.empty
  where go (TyCon _) = id
        go (TyVar _) = id
        go (TyAp tau1 tau2) = go tau1 . go tau2
        go (TyMeta tv) = S.insert tv

getMetaVars :: [Type] -> TI (S.Set TyMeta)
getMetaVars = fmap (S.unions . map metaVars) . mapM zonk
