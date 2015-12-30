module Core.FreeVars
( freeVars
) where

import Common

import Core.AST
import Core.AnnotAST
import qualified Data.Set as S

freeVars :: Program Name -> AnnotProgram (S.Set Name) Name
freeVars = map freeVarsSC
  where freeVarsSC (name, args, body) = (name, args, body')
          where body' = freeVarsExpr (S.fromList args) body

freeVarsExpr :: S.Set Name -> Expr Name -> AnnotExpr (S.Set Name) Name
freeVarsExpr vars e = Annot $ case e of
  EVar v -> if S.member v vars
    then (S.singleton v, EVarF v)
    else (S.empty, EVarF v)
  ENum n -> (S.empty, ENumF n)
  EConstr tag arity -> (S.empty, EConstrF tag arity)
  EAp e1 e2 -> (S.union (fv e1') (fv e2'), EApF e1' e2')
    where e1' = freeVarsExpr vars e1
          e2' = freeVarsExpr vars e2
  ELet rec defs body -> (S.union defsFV bodyFV, ELetF rec (zip xs exps') body')
    where exps' = map (freeVarsExpr (if rec then vars' else vars)) exps
          body' = freeVarsExpr vars' body
          vars' = S.union vars (S.fromList xs)
          defsFV = (if rec then bind else id) (S.unions (map fv exps'))
          bodyFV = bind (fv body')
          bind = (S.\\ S.fromList xs)
          xs = map fst defs
          exps = map snd defs
  ECase e alts -> (S.union (fv e') altsFV, ECaseF e' alts')
    where e' = freeVarsExpr vars e
          alts' = map (freeVarsAlter vars) alts
          altsFV = S.unions (map bind alts')
          bind (_, xs, body) = fv body S.\\ S.fromList xs
  EAbs args body -> (bodyFV, EAbsF args body')
    where body' = freeVarsExpr vars' body
          vars' = S.union vars (S.fromList args)
          bodyFV = fv body' S.\\ S.fromList args
  where fv = fst . unAnnot

freeVarsAlter :: S.Set Name -> Alter Name -> AnnotAlter (S.Set Name) Name
freeVarsAlter vars (tag, xs, body) = (tag, xs, body')
  where body' = freeVarsExpr vars' body
        vars' = S.union vars (S.fromList xs)
