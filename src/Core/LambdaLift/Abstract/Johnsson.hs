{-# LANGUAGE ViewPatterns #-}

module Core.LambdaLift.Abstract.Johnsson
( abstract
) where

import Common

import Core.AST
import Core.AnnotAST
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

abstract :: AnnotProgram (S.Set Name) Name -> Program Name
abstract = Program . map abstractSC . getProgramF
  where abstractSC (SupercombF name args body) = Supercomb name args body'
          where body' = abstractExpr M.empty body

type AbsEnv = M.Map Name [Name]

mkAbs :: AbsEnv -> [Name] -> AnnotExpr (S.Set Name) Name -> Expr Name
mkAbs env fv (unAnnot -> EAbsF args body) = EAbs (fv ++ args) body'
  where body' = abstractExpr env body

abstractExpr :: AbsEnv -> AnnotExpr (S.Set Name) Name -> Expr Name
abstractExpr env a@(Annot (fv, e)) = case e of
  EVarF v -> foldl EAp (EVar v) (map EVar (M.findWithDefault [] v env))
  ENumF n -> ENum n
  EConstrF tag arity -> EConstr tag arity
  EApF e1 e2 -> EAp (abstractExpr env e1) (abstractExpr env e2)
  -- TODO: eliminate unused arguments
  ELetF rec defs body -> ELet rec (lambdas' ++ vars') body'
    where (lambdas, vars) = partition (isAbs . removeAnnot . snd) defs
          lambdas' = zip xs (map (mkAbs env'' fv') exps)
          vars' = [(x, abstractExpr env'' e) | (x, e) <- vars]
          body' = abstractExpr env' body
          env' = extend env (zip xs (repeat fv'))
          env'' = if rec then env' else env
          fv' = abstractFreeVars env (bind (S.unions (map getAnnot exps)))
          bind = (S.\\ S.fromList xs)
          (xs, exps) = unzip lambdas
  ECaseF e alts -> ECase (abstractExpr env e) (map (abstractAlter env) alts)
  EAbsF args body -> foldl EAp e' (map EVar fv')
    where e' = ELet False [(anonym, mkAbs env fv' a)] (EVar anonym)
          fv' = abstractFreeVars env fv

abstractAlter :: AbsEnv -> AnnotAlter (S.Set Name) Name -> Alter Name
abstractAlter env (AlterF tag xs body) = Alter tag xs (abstractExpr env body)

abstractFreeVars :: AbsEnv -> S.Set Name -> [Name]
abstractFreeVars env fv = S.toList (S.unions xs)
  where xs = [S.fromList (M.findWithDefault [v] v env) | v <- S.toList fv]
