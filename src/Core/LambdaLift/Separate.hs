module Core.LambdaLift.Separate
( separate
) where

import Common

import Core.AST

separate :: Program Name -> Program Name
separate = Program . map separateSC . getProgram
  where separateSC (Supercomb name args body) = Supercomb name [] body'
          where body' = mkAbs args (separateExpr body)

mkAbs :: [Name] -> Expr Name -> Expr Name
mkAbs args body = foldr (\arg -> EAbs [arg]) body args

separateExpr :: Expr Name -> Expr Name
separateExpr e = case e of
  EVar _ -> e
  ENum _ -> e
  EConstr _ _ -> e
  EAp e1 e2 -> EAp (separateExpr e1) (separateExpr e2)
  ELet rec defs body -> ELet rec (zip xs exps) (separateExpr body)
    where xs = map fst defs
          exps = map (separateExpr . snd) defs
  ECase e alts -> ECase e' alts'
    where e' = separateExpr e
          alts' = map separateAlter alts
  EAbs args body -> mkAbs args (separateExpr body)

separateAlter :: Alter Name -> Alter Name
separateAlter (Alter tag xs body) = Alter tag xs (separateExpr body)
