module Type.Infer
( infer
) where

import Control.Monad
import Data.IORef
import Data.NameSupply
import qualified Data.Map as M
import qualified Data.Set as S
import Text.PrettyPrint
import Type.Kind
import Type.Meta
import Type.PrettyPrint
import Type.Subst
import Type.Types
import Type.Unify

infer :: Term -> TI Sigma
infer e = inferSigma e >>= zonk

data Delta a
  = Check a
  | Infer (IORef a)

inferRho :: Term -> TI Rho
inferRho e = do
  ref <- newTIRef (error "Empty rho")
  tiRho e (Infer ref)
  readTIRef ref

checkRho :: Term -> Rho -> TI ()
checkRho e = tiRho e . Check

-- bidirectional type inference
tiRho :: Term -> Delta Rho -> TI ()
tiRho (TmVar name) delta = do
  sigma <- lookupTyEnv name
  instSigma sigma delta
tiRho (TmLit _) delta = instSigma (tauToSigma (TyCon "Int")) delta
tiRho (TmAp e1 e2) delta = do
  rho1 <- inferRho e1
  (sigma2, rho) <- unifyTyArr rho1
  checkSigma e2 sigma2
  instSigma (rhoToSigma rho) delta
tiRho (TmAbs name body) delta = case delta of
  Check rho -> do
    (sigma, rho') <- unifyTyArr rho
    extendTyEnv name sigma (checkRho body rho')
  Infer ref -> do
    sigma <- fmap tauToSigma newTyVar
    rho <- extendTyEnv name sigma (inferRho body)
    writeTIRef ref (TyArr sigma (rhoToSigma rho))
tiRho (TmAnnotAbs name sigma body) delta = do
  checkKind sigma KnStar
  case delta of
    Check rho -> do
      (sigma', rho') <- unifyTyArr rho
      subsCheck sigma' sigma
      extendTyEnv name sigma (checkRho body rho')
    Infer ref -> do
      rho <- extendTyEnv name sigma (inferRho body)
      writeTIRef ref (TyArr sigma (rhoToSigma rho))
tiRho (TmLet name e body) delta = do
  sigma <- inferSigma e
  extendTyEnv name sigma (tiRho body delta)
tiRho (TmAnnot e sigma) delta = do
  checkKind sigma KnStar
  checkSigma e sigma
  instSigma sigma delta
-- TODO: add support for branches, patterns, etc.

inferSigma :: Term -> TI Sigma
inferSigma e = do
  rho <- inferRho e
  tvsEnv <- gets tyEnv >>= getMetaVars . M.elems
  tvsRho <- getMetaVars rho
  quantify (S.toList (tvsRho S.\\ tvsEnv)) rho

checkSigma :: Term -> Sigma -> TI ()
checkSigma e sigma = do
  (sks, rho) <- skolemise sigma
  checkRho e rho
  tvs <- gets tyEnv >>= getFreeVars . (sigma:) . M.elems
  let sks' = filter (\tv -> S.member tv tvs) sks
  unless (null sks') (throwError "Type not polymorphic enough")

subsCheck :: Sigma -> Sigma -> TI ()
subsCheck sigma1 sigma2 = do
  (sks, rho2) <- skolemise sigma2
  subsCheckRho sigma1 rho2
  tvs <- getFreeVars [sigma1, sigma2]
  let sks' = filter (\tv -> S.member tv tvs) sks
  unless (null sks') . throwTI $ hsep
    [ text "Subsumption check failed:"
    , pprint sigma1
    , text "is not as polymorphic as"
    , pprint sigma2 ]

subsCheckRho :: Sigma -> Rho -> TI ()
subsCheckRho sigma1 rho2 = do
  rho1 <- instantiate sigma1
  case (rho1, rho2) of
    (_, TyArr sigma21 sigma22) -> do
      (sigma11, rho12) <- unifyTyArr rho1
      subsCheckArr sigma11 rho12 sigma21 (sigmaToRho sigma22)
    (TyArr sigma11 sigma12, _) -> do
      (sigma21, rho22) <- unifyTyArr rho2
      subsCheckArr sigma11 (sigmaToRho sigma12) sigma21 rho22
    (TyMono tau1, TyMono tau2) -> unify tau1 tau2

subsCheckArr :: Sigma -> Rho -> Sigma -> Rho -> TI ()
subsCheckArr sigma1 rho1 sigma2 rho2 = do
  subsCheck sigma2 sigma1 -- contravariance
  subsCheckRho (rhoToSigma rho1) rho2

instSigma :: Sigma -> Delta Rho -> TI ()
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer ref) = instantiate sigma >>= writeTIRef ref

instantiate :: Sigma -> TI Rho
instantiate (TyForall tvs rho) = do
  tvs' <- mapM (const newMeta) tvs
  return (subst_ tvs (map TyMeta tvs') rho)

quantify :: [TyMeta] -> Rho -> TI Sigma
quantify tvs rho = do
  mapM_ bind (zip tvs tvs')
  rho' <- zonk rho
  return (TyForall tvs' rho')
  where usedNames = S.map getTyVarName (boundTyVars rho)
        freshNames = getNames (mkNameSupply usedNames)
        tvs' = take (length tvs) (map Bound freshNames)
        bind (tv, tv') = writeMeta tv (TyVar tv')

boundTyVars :: Rho -> S.Set TyVar
boundTyVars rho = go rho S.empty
  where go (TyMono _) = id
        go (TyArr sigma1 sigma2) = go' sigma1 . go' sigma2
        go' (TyForall tvs rho) acc = foldr S.insert acc tvs
