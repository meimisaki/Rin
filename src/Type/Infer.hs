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

infer :: Term -> TI Type
infer e = inferSigma e >>= zonk

data Delta a
  = Check a
  | Infer (IORef a)

inferRho :: Term -> TI Rho
inferRho e = do
  ref <- newTIRef (error "Panic: empty rho")
  tiRho e (Infer ref)
  readTIRef ref

checkRho :: Term -> Rho -> TI ()
checkRho e = tiRho e . Check

-- bidirectional type inference
tiRho :: Term -> Delta Rho -> TI ()
tiRho (TmVar name) delta = do
  ty <- lookupTyEnv name
  instSigma ty delta
tiRho (TmLit _) delta = instSigma (TyCon "Int") delta
tiRho (TmAp e1 e2) delta = do
  rho1 <- inferRho e1
  (sigma2, rho) <- unifyArr rho1
  checkSigma e2 sigma2
  instSigma rho delta
tiRho (TmAbs name body) delta = case delta of
  Check rho -> do
    (sigma, rho') <- unifyArr rho
    extendTyEnv name sigma (checkRho body rho')
  Infer ref -> do
    ty <- newTyVar
    rho <- extendTyEnv name ty (inferRho body)
    writeTIRef ref (TyArr ty rho)
tiRho (TmAnnotAbs name ty body) delta = do
  checkKind ty KnStar
  case delta of
    Check rho -> do
      (sigma, rho') <- unifyArr rho
      subsCheck sigma ty
      extendTyEnv name ty (checkRho body rho')
    Infer ref -> do
      rho <- extendTyEnv name ty (inferRho body)
      writeTIRef ref (TyArr ty rho)
tiRho (TmLet name e body) delta = do
  sigma <- inferSigma e
  extendTyEnv name sigma (tiRho body delta)
tiRho (TmAnnot e ty) delta = do
  checkKind ty KnStar
  checkSigma e ty
  instSigma ty delta
-- TODO: add support for branches, patterns, etc.

inferSigma :: Term -> TI Sigma
inferSigma e = do
  rho <- inferRho e
  tvsEnv <- gets tyEnv >>= getMetaVars . M.elems
  tvsRho <- getMetaVars [rho]
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
subsCheckRho sigma1@(TyForall _ _) rho2 = do
  rho1 <- instantiate sigma1
  subsCheckRho rho1 rho2
subsCheckRho rho (TyArr sigma2 rho2) = do
  (sigma1, rho1) <- unifyArr rho
  subsCheckArr sigma1 rho1 sigma2 rho2
subsCheckRho (TyArr sigma1 rho1) rho = do
  (sigma2, rho2) <- unifyArr rho
  subsCheckArr sigma1 rho1 sigma2 rho2
subsCheckRho tau1 tau2 = unify tau1 tau2

subsCheckArr :: Sigma -> Rho -> Sigma -> Rho -> TI ()
subsCheckArr sigma1 rho1 sigma2 rho2 = do
  subsCheck sigma2 sigma1 -- contravariance
  subsCheckRho rho1 rho2

instSigma :: Sigma -> Delta Rho -> TI ()
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer ref) = instantiate sigma >>= writeTIRef ref

instantiate :: Sigma -> TI Rho
instantiate (TyForall tvs rho) = do
  tvs' <- mapM (const newTyVar) tvs
  return (subst_ tvs tvs' rho)
instantiate ty = return ty

quantify :: [TyMeta] -> Rho -> TI Sigma
quantify tvs rho = do
  mapM_ bind (zip tvs tvs')
  rho' <- zonk rho
  return (mkForall tvs' rho')
  where usedNames = S.map getTyVarName (boundTyVars rho)
        freshNames = getNames (mkNameSupply usedNames)
        tvs' = take (length tvs) (map Bound freshNames)
        bind (tv, tv') = writeMeta tv (TyVar tv')

boundTyVars :: Rho -> S.Set TyVar
boundTyVars rho = go rho S.empty
  where go (TyForall tvs rho) acc = foldr S.insert acc tvs
        go (TyArr sigma1 sigma2) acc = go sigma1 (go sigma2 acc)
        go _ acc = acc
