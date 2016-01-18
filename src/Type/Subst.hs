module Type.Subst
( skolemise
, subst
, subst_
, freeVars
, getFreeVars
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Type.Meta
import Type.Types

skolemise :: Sigma -> TI ([TyVar], Rho)
skolemise (TyForall tvs rho) = do
  sks1 <- mapM newSkolemTyVar tvs
  (sks2, rho') <- skolemise (subst_ tvs (map TyVar sks1) rho)
  return (sks1 ++ sks2, rho')
skolemise (TyArr sigma1 sigma2) = do
  (sks, rho2) <- skolemise sigma2
  return (sks, TyArr sigma1 rho2)
skolemise tau = return ([], tau)

subst :: M.Map TyVar Tau -> Type -> Type
subst env (TyForall tvs rho) = mkForall tvs (subst env' rho)
  where env' = foldr M.delete env tvs
subst env (TyArr sigma1 sigma2) = TyArr (subst env sigma1) (subst env sigma2)
subst env tau@(TyCon _) = tau
subst env tau@(TyVar tv) = M.findWithDefault tau tv env
subst env (TyAp tau1 tau2) = TyAp (subst env tau1) (subst env tau2)
subst env tau@(TyMeta _) = tau

subst_ :: [TyVar] -> [Tau] -> Type -> Type
subst_ tvs = subst . M.fromList . zip tvs

freeVars :: Type -> S.Set TyVar
freeVars (TyForall tvs rho) = freeVars rho S.\\ S.fromList tvs
freeVars (TyArr sigma1 sigma2) = S.union tvs1 tvs2
  where tvs1 = freeVars sigma1
        tvs2 = freeVars sigma2
freeVars tau = go tau S.empty
  where go (TyCon _) = id
        go (TyVar tv) = S.insert tv
        go (TyAp tau1 tau2) = go tau1 . go tau2
        go (TyMeta _) = id

getFreeVars :: [Type] -> TI (S.Set TyVar)
getFreeVars = fmap (S.unions . map freeVars) . mapM zonk
