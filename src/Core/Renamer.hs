module Core.Renamer
( rename
) where

import Common

import Control.Monad.State
import Core.AST
import Data.NameSupply
import qualified Data.Map as M
import qualified Data.Set as S

type NS = State NameSupply

runNS :: NS a -> NameSupply -> a
runNS state ns = fst (runState state ns)

newName :: NS Name
newName = do
  ns <- get
  let (name, ns') = getFreshName ns
  put ns'
  return name

rename :: Program Name -> Program Name
rename (Program prog) = Program (runNS (mapM renameSC prog) ns)
  where ns = mkNameSupply (S.fromList [name | (Supercomb name _ _) <- prog])
        renameSC (Supercomb name args body) = do
          (env, args') <- renameArgs M.empty args
          body' <- renameExpr env body
          return (Supercomb name args' body')

type RenameEnv = M.Map Name Name

renameArgs :: RenameEnv -> [Name] -> NS (RenameEnv, [Name])
renameArgs env args = do
  args' <- mapM (const newName) args
  let env' = extend env (zip args args')
  return (env', args')

renameExpr :: RenameEnv -> Expr Name -> NS (Expr Name)
renameExpr env e = case e of
  EVar v -> return (EVar (M.findWithDefault v v env))
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

renameAlter :: RenameEnv -> Alter Name -> NS (Alter Name)
renameAlter env (Alter tag xs body) = do
  (env', xs') <- renameArgs env xs
  body' <- renameExpr env' body
  return (Alter tag xs' body')
