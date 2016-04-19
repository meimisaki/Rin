{-# LANGUAGE RecordWildCards #-}

module Core.Rename
( rename
, rename_
) where

import Common

import Core.AST
import Data.NameSupply
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Set as S

rename_ :: Program Name -> Program Name
rename_ = fmap runIdentity . rename runIdentity . fmap Identity

rename :: Functor f => (f Name -> Name) -> Program (f Name) -> Program (f Name)
rename nameOf (Program prog) = Program (evalNS (mapM renameSC prog) ns)
  where ns = mkNameSupply (S.fromList [name | (Supercomb name _ _) <- prog])
        renameSC (Supercomb name args body) = do
          (env, args') <- renameArgs (RenameEnv M.empty nameOf) args
          body' <- renameExpr env body
          return (Supercomb name args' body')

data RenameEnv f = RenameEnv
  { subst :: M.Map Name Name
  , nameOf :: f Name -> Name }

renameArgs :: Functor f => RenameEnv f -> [f Name] -> NS (RenameEnv f, [f Name])
renameArgs env@(RenameEnv {..}) args = do
  let names = map nameOf args
  names' <- mapM (const newName) names
  let env' = env { subst = extend subst (zip names names') }
      args' = map (\(arg, name) -> fmap (const name) arg) (zip args names')
  return (env', args')

renameExpr :: Functor f => RenameEnv f -> Expr (f Name) -> NS (Expr (f Name))
renameExpr env@(RenameEnv {..}) e = case e of
  EVar v -> return (EVar (M.findWithDefault v v subst))
  ENum _ -> return e
  EConstr _ _ -> return e
  EAp e1 e2 -> do
    e1' <- renameExpr env e1
    e2' <- renameExpr env e2
    return (EAp e1' e2')
  ELet rec defs body -> do
    let xs = map fst defs
        exps = map snd defs
    (env', xs') <- renameArgs env xs
    exps' <- mapM (renameExpr (if rec then env' else env)) exps
    body' <- renameExpr env' body
    return (ELet rec (zip xs' exps') body')
  ECase e alts -> do
    e' <- renameExpr env e
    alts' <- mapM (renameAlter env) alts
    return (ECase e' alts')
  EAbs args body -> do
    (env', args') <- renameArgs env args
    body' <- renameExpr env' body
    return (EAbs args' body')

renameAlter :: Functor f => RenameEnv f -> Alter (f Name) -> NS (Alter (f Name))
renameAlter env (Alter tag xs body) = do
  (env', xs') <- renameArgs env xs
  body' <- renameExpr env' body
  return (Alter tag xs' body')
