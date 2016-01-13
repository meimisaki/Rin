module Type.Class
( Type (..)
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Type.Types

class Type a where
  zonk :: a -> TI a
  skolemise :: a -> TI ([TyVar], Rho)
  subst :: M.Map TyVar Tau -> a -> a
  subst_ :: [TyVar] -> [Tau] -> a -> a
  metaTyVars :: a -> S.Set TyMeta
  freeTyVars :: a -> S.Set TyVar
  getMetaTyVars :: a -> TI (S.Set TyMeta)
  getFreeTyVars :: a -> TI (S.Set TyVar)
  {-# MINIMAL zonk, skolemise, (subst | subst_), metaTyVars, freeTyVars #-}
  subst = uncurry subst_ . unzip . M.toList
  subst_ tvs = subst . M.fromList . zip tvs
  getMetaTyVars = fmap metaTyVars . zonk
  getFreeTyVars = fmap freeTyVars . zonk

instance Type a => Type [a] where
  zonk = mapM zonk
  skolemise _ = throwError "Can not skolemise [a]"
  subst = map . subst
  metaTyVars = S.unions . map metaTyVars
  freeTyVars = S.unions . map freeTyVars

instance Type Sigma where
  zonk (TyForall tvs rho) = do
    rho' <- zonk rho
    return (TyForall tvs rho')
  skolemise (TyForall tvs rho) = do
    sks1 <- mapM newSkolemTyVar tvs
    (sks2, rho') <- skolemise (subst_ tvs (map TyVar sks1) rho)
    return (sks1 ++ sks2, rho')
  subst env (TyForall tvs rho) = TyForall tvs (subst env' rho)
    where env' = foldr M.delete env tvs
  metaTyVars (TyForall _ rho) = metaTyVars rho
  freeTyVars (TyForall tvs rho) = freeTyVars rho S.\\ S.fromList tvs

instance Type Rho where
  zonk (TyMono tau) = fmap TyMono (zonk tau)
  zonk (TyArr sigma1 sigma2) = do
    sigma1' <- zonk sigma1
    sigma2' <- zonk sigma2
    return (TyArr sigma1' sigma2')
  skolemise (TyMono tau) = skolemise tau
  skolemise (TyArr sigma1 sigma2) = do
    (sks, rho2) <- skolemise sigma2
    return (sks, TyArr sigma1 (rhoToSigma rho2))
  subst env (TyMono tau) = TyMono (subst env tau)
  subst env (TyArr sigma1 sigma2) = TyArr (subst env sigma1) (subst env sigma2)
  metaTyVars (TyMono tau) = metaTyVars tau
  metaTyVars (TyArr sigma1 sigma2) = S.union tvs1 tvs2
    where tvs1 = metaTyVars sigma1
          tvs2 = metaTyVars sigma2
  freeTyVars (TyMono tau) = freeTyVars tau
  freeTyVars (TyArr sigma1 sigma2) = S.union tvs1 tvs2
    where tvs1 = freeTyVars sigma1
          tvs2 = freeTyVars sigma2

instance Type Tau where
  zonk tau@(TyCon _) = return tau
  zonk tau@(TyVar _) = return tau
  zonk (TyArr0 tau1 tau2) = do
    tau1' <- zonk tau1
    tau2' <- zonk tau2
    return (TyArr0 tau1' tau2')
  zonk tau@(TyMeta tv) = do
    mbTau1 <- readTyMeta tv
    case mbTau1 of
      Nothing -> return tau
      Just tau1 -> do
        tau2 <- zonk tau1
        writeTyMeta tv tau2
        return tau2
  skolemise tau = return ([], TyMono tau)
  subst env tau@(TyCon _) = tau
  subst env tau@(TyVar tv) = M.findWithDefault tau tv env
  subst env (TyArr0 tau1 tau2) = TyArr0 (subst env tau1) (subst env tau2)
  subst env tau@(TyMeta _) = tau
  metaTyVars tau = go tau S.empty
    where go (TyCon _) = id
          go (TyVar _) = id
          go (TyArr0 tau1 tau2) = go tau1 . go tau2
          go (TyMeta tv) = S.insert tv
  freeTyVars tau = go tau S.empty
    where go (TyCon _) = id
          go (TyVar tv) = S.insert tv
          go (TyArr0 tau1 tau2) = go tau1 . go tau2
          go (TyMeta _) = id
