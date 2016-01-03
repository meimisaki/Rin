module Core.LambdaLift.Collect
( collect
) where

import Common

import Core.AST
import Data.List

collect :: Program Name -> Program Name
collect = Program . concatMap (collectSC . elim) . getProgram
  where collectSC (Supercomb name args body) = Supercomb name args body':scDefs
          where (scDefs, body') = collectExpr body
        -- eliminate redundant supercombinators
        elim sc@(Supercomb name args1 (ELet _ [(x, EAbs args2 body)] (EVar v)))
          | x == v = Supercomb name (args1 ++ args2) body
          | otherwise = sc
        elim sc = sc

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
    where (defsSC, exps') = accum (map collectExpr exps)
          (bodySC, body') = collectExpr body
          (lambdas, defs') = partition (isAbs . snd) (zip xs exps')
          scDefs = [Supercomb name args body | (name, EAbs args body) <- lambdas]
          xs = map fst defs
          exps = map snd defs
  ECase e alts -> (scDefs ++ altsSC, ECase e' alts')
    where (scDefs, e') = collectExpr e
          (altsSC, alts') = accum (map collectAlter alts)
  EAbs args body -> (scDefs, EAbs args body')
    where (scDefs, body') = collectExpr body

collectAlter :: Alter Name -> ([Supercomb Name], Alter Name)
collectAlter (Alter tag xs body) = (scDefs, Alter tag xs body')
  where (scDefs, body') = collectExpr body
