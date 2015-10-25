{-# LANGUAGE FlexibleInstances #-}

module Infer where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

import AST

type Subst = M.Map TVar Type

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (apply s1) s2 `M.union` s1

class Substitutable a where
  ftv :: a -> S.Set TVar
  apply :: Subst -> a -> a

instance Substitutable a => Substitutable [a] where
  ftv xs = foldr S.union S.empty (map ftv xs)
  apply s = map (apply s)

instance Substitutable Type where
  ftv (TVar x) = S.singleton x
  ftv (TCon _) = S.empty
  ftv (TApp t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TForall vars t) = ftv t S.\\ S.fromList vars
  apply s t@(TVar x) = M.findWithDefault t x s
  apply s t@(TCon _) = t
  apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)
  apply s (TForall vars t) = TForall vars (apply (foldr M.delete s vars) t)

type TypeEnv = M.Map EVar Type

instance Substitutable TypeEnv where
  ftv = ftv . M.elems
  apply s = M.map (apply s)

generalize :: TypeEnv -> Type -> Type
generalize env t = TForall vars t
  where vars = S.toList (ftv t S.\\ ftv env)

type TIError = String

data TIEnv = TIEnv {}

data TIState = TIState { supply :: Int }

type TI a = ExceptT TIError (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either TIError a, TIState)
runTI t = runStateT (runReaderT (runExceptT t) env) state
  where env = TIEnv
        state = TIState 0

newTyVar :: TVar -> TI Type
newTyVar pr = do
  s <- get
  put s { supply = supply s + 1 }
  return $ TVar (pr ++ show (supply s))

(|=>) :: TVar -> Type -> TI Subst
x |=> t
  | TVar x == t = return nullSubst
  | x `S.member` ftv t = throwError $ "occur-check fails: " ++ x ++ " vs. " ++ show t
  | otherwise = return $ M.singleton x t

instantiate :: Type -> TI Type
instantiate (TForall vars t) = do
  nvars <- mapM (const (newTyVar "a")) vars
  let s = M.fromList (zip vars nvars)
  return $ apply s t
instantiate t = return t

unifyError :: Type -> Type -> TI Subst
unifyError t1 t2 = throwError $ "can not unify types: " ++ show t1 ++ " vs. " ++ show t2

unify :: Type -> Type -> TI Subst
unify (TVar x) t = x |=> t
unify t (TVar x) = x |=> t
unify t1@(TCon a) t2@(TCon b)
  | a == b = return nullSubst
  | otherwise = unifyError t1 t2
unify (TApp l r) (TApp l' r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return $ s2 `composeSubst` s1
unify t1 t2 = unifyError t1 t2

tiLit :: TypeEnv -> Literal -> TI (Subst, Type)
tiLit _ (LBool _) = return (nullSubst, TCon "Bool")
tiLit _ (LInt _) = return (nullSubst, TCon "Int")

ti :: TypeEnv -> Exp -> TI (Subst, Type)
ti env (EVar x) = case M.lookup x env of
  Nothing -> throwError $ "unbound variable: " ++ x
  Just sigma -> do
    t <- instantiate sigma
    return (nullSubst, t)
ti env (ELit x) = tiLit env x
ti env (EAbs x e) = do
  tv <- newTyVar "a"
  (s, t) <- ti (M.insert x tv env) e
  return (s, TArr (apply s tv) t)
ti env (EApp e1 e2) = do
  tv <- newTyVar "a"
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  s3 <- unify (apply s2 t1) (TArr t2 tv)
  return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (EFix xs e) = do
  nvars <- mapM (const (newTyVar "a")) vars
  (s1, ts) <- foldrM (ti' (bind nvars)) (nullSubst, []) (zip exps nvars)
  let gts = map (generalize (apply s1 env)) (apply s1 ts)
  (s2, t) <- ti (apply s1 (bind gts)) e
  return (s2 `composeSubst` s1, t)
  where (vars, exps) = unzip xs
        bind = foldr (uncurry M.insert) env . zip vars
        ti' env (e, tv) (s1, ts) = do
          (s2, t) <- ti (apply s1 env) e
          s3 <- unify tv t
          return (s3 `composeSubst` s2 `composeSubst` s1, t:ts)

infer :: TypeEnv -> Exp -> TI Type
infer env e = liftM snd (ti env e)
