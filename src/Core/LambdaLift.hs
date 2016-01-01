{-# LANGUAGE ViewPatterns #-}

module Core.LambdaLift
( lambdaLift
) where

import Common

import Core.AST
import Core.AnnotAST
import Core.FreeVars
import Core.Rename
import Data.List
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
          anonym = ""

abstractAlter :: AnnotAlter (S.Set Name) Name -> Alter Name
abstractAlter (AlterF tag xs body) = Alter tag xs (abstractExpr body)

collect :: Program Name -> Program Name
collect = Program . concatMap collectSC . getProgram
  where collectSC (Supercomb name args body) = Supercomb name args body':scDefs
          where (scDefs, body') = collectExpr body

mkLet :: Bool -> [(a, Expr a)] -> Expr a -> Expr a
mkLet _ [] body = body
mkLet rec defs body = ELet rec defs body

collectExpr :: Expr Name -> ([Supercomb Name], Expr Name)
collectExpr e = case e of
  EVar _ -> ([], e)
  ENum _ -> ([], e)
  EConstr _ _ -> ([], e)
  EAp e1 e2 -> (scDefs1 ++ scDefs2, EAp e1' e2')
    where (scDefs1, e1') = collectExpr e1
          (scDefs2, e2') = collectExpr e2
  ELet rec defs body -> (defsSC ++ bodySC ++ scDefs, mkLet rec defs' body')
    where (defsSC, exps') = merge (map collectExpr exps)
          (bodySC, body') = collectExpr body
          (lambdas, defs') = partition (isAbs . snd) (zip xs exps')
          scDefs = [Supercomb name args body | (name, EAbs args body) <- lambdas]
          xs = map fst defs
          exps = map snd defs
          isAbs (EAbs _ _) = True
          isAbs _ = False
  ECase e alts -> (scDefs ++ altsSC, ECase e' alts')
    where (scDefs, e') = collectExpr e
          (altsSC, alts') = merge (map collectAlter alts)
  EAbs args body -> (scDefs, EAbs args body')
    where (scDefs, body') = collectExpr body
  where merge xs = (concatMap fst xs, map snd xs)

collectAlter :: Alter Name -> ([Supercomb Name], Alter Name)
collectAlter (Alter tag xs body) = (scDefs, Alter tag xs body')
  where (scDefs, body') = collectExpr body

-- TODO: eta-abstraction optimize, merge directly nested lambdas
lambdaLift :: Program Name -> Program Name
lambdaLift = collect . rename_ . abstract . freeVars
