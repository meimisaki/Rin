module Type.Subst
( Subst (..)
, getFreeVars
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Type.Meta
import Type.Types

class Subst a where
  skolemise :: a -> TI ([TyVar], Rho)
  subst :: M.Map TyVar Tau -> a -> a
  subst_ :: [TyVar] -> [Tau] -> a -> a
  freeVars :: a -> S.Set TyVar
  {-# MINIMAL skolemise, (subst | subst_), freeVars #-}
  subst = uncurry subst_ . unzip . M.toList
  subst_ tvs = subst . M.fromList . zip tvs

getFreeVars :: (Meta a, Subst a) => a -> TI (S.Set TyVar)
getFreeVars = fmap freeVars . zonk

instance Subst a => Subst [a] where
  skolemise _ = throwError "Can not skolemise [a]"
  subst = map . subst
  freeVars = S.unions . map freeVars

instance Subst Sigma where
  skolemise (TyForall tvs rho) = do
    sks1 <- mapM newSkolemTyVar tvs
    (sks2, rho') <- skolemise (subst_ tvs (map TyVar sks1) rho)
    return (sks1 ++ sks2, rho')
  subst env (TyForall tvs rho) = TyForall tvs (subst env' rho)
    where env' = foldr M.delete env tvs
  freeVars (TyForall tvs rho) = freeVars rho S.\\ S.fromList tvs

instance Subst Rho where
  skolemise (TyMono tau) = skolemise tau
  skolemise (TyArr sigma1 sigma2) = do
    (sks, rho2) <- skolemise sigma2
    return (sks, TyArr sigma1 (rhoToSigma rho2))
  subst env (TyMono tau) = TyMono (subst env tau)
  subst env (TyArr sigma1 sigma2) = TyArr (subst env sigma1) (subst env sigma2)
  freeVars (TyMono tau) = freeVars tau
  freeVars (TyArr sigma1 sigma2) = S.union tvs1 tvs2
    where tvs1 = freeVars sigma1
          tvs2 = freeVars sigma2

instance Subst Tau where
  skolemise tau = return ([], TyMono tau)
  subst env tau@(TyCon _) = tau
  subst env tau@(TyVar tv) = M.findWithDefault tau tv env
  subst env (TyAp tau1 tau2) = TyAp (subst env tau1) (subst env tau2)
  subst env tau@(TyMeta _) = tau
  freeVars tau = go tau S.empty
    where go (TyCon _) = id
          go (TyVar tv) = S.insert tv
          go (TyAp tau1 tau2) = go tau1 . go tau2
          go (TyMeta _) = id
