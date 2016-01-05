{-# LANGUAGE ViewPatterns #-}

module Core.LambdaLift.Abstract.Naive
( abstract
) where

import Common

import Core.AST
import Core.AnnotAST
import qualified Data.Set as S

abstract :: AnnotProgram (S.Set Name) Name -> Program Name
abstract = Program . map abstractSC . getProgramF
  where abstractSC (SupercombF name args body) = Supercomb name args (abstractExpr body)

abstractExpr :: AnnotExpr (S.Set Name) Name -> Expr Name
abstractExpr (Annot (S.toList -> fv, e)) = case e of
  EVarF v -> EVar v
  ENumF n -> ENum n
  EConstrF tag arity -> EConstr tag arity
  EApF e1 e2 -> EAp (abstractExpr e1) (abstractExpr e2)
  ELetF rec defs body -> ELet rec defs' body'
    where defs' = [(x, abstractExpr e) | (x, e) <- defs]
          body' = abstractExpr body
  ECaseF e alts -> ECase (abstractExpr e) (map abstractAlter alts)
  EAbsF args body -> foldl EAp e' (map EVar fv)
    where e' = ELet False [(anonym, EAbs (fv ++ args) body')] (EVar anonym)
          body' = abstractExpr body

abstractAlter :: AnnotAlter (S.Set Name) Name -> Alter Name
abstractAlter (AlterF tag xs body) = Alter tag xs (abstractExpr body)
