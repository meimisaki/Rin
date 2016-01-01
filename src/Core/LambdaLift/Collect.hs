module Core.LambdaLift.Collect
( collect
) where

import Common

import Core.AST
import Data.List

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
