module Core.LambdaLift.DeBruijn
( deBruijn
) where

import Common

import Core.AST
import Core.AnnotAST
import Core.FreeVars
import Core.Prelude
import qualified Data.Map as M
import qualified Data.Set as S

deBruijn :: Program Name -> AnnotProgram Int (Annot Int Name)
deBruijn = ProgramF . map deBruijnSC . getProgramF . freeVars
  where deBruijnSC (SupercombF name [] body) = SupercombF name [] body'
          where body' = deBruijnExpr 0 M.empty body

deBruijnExpr :: Int -> M.Map Name Int -> AnnotExpr (S.Set Name) Name -> AnnotExpr Int (Annot Int Name)
deBruijnExpr k env (Annot (fv, e)) = Annot $ case e of
  EVarF v -> (M.findWithDefault 0 v env, EVarF v)
  ENumF n -> (0, ENumF n)
  EConstrF tag arity -> (0, EConstrF tag arity)
  EApF e1 e2 -> (,) k' $ if isArith (EAp (removeAnnot e1) (removeAnnot e2))
    then EApF (Annot (k', unAnnot e1')) e2'
    else EApF e1' e2'
    where e1' = deBruijnExpr k env e1
          e2' = deBruijnExpr k env e2
          k' = max (getAnnot e1') (getAnnot e2')
  ELetF rec defs body -> (getAnnot body', ELetF rec defs' body')
    where xs' = map (Annot . (,) k') xs
          exps' = map (deBruijnExpr k (if rec then env' else env)) exps
          defs' = zip xs' exps'
          body' = deBruijnExpr k env' body
          env' = extend env (zip xs (repeat k'))
          env'' = if rec then extend env (zip xs (repeat 0)) else env
          k' = deBruijnFreeVars env'' (S.unions (map getAnnot exps))
          (xs, exps) = unzip defs
  ECaseF e alts -> (k'', ECaseF e' alts')
    where e' = deBruijnExpr k env e
          alts' = map (deBruijnAlter k k' env) alts
          k' = getAnnot e'
          k'' = foldr max k' [getAnnot body | AlterF _ _ body <- alts']
  EAbsF args body -> (deBruijnFreeVars env fv, EAbsF args' body')
    where args' = map (Annot . (,) k') args
          body' = deBruijnExpr k' env' body
          env' = extend env (zip args (repeat k'))
          k' = k + 1

deBruijnAlter :: Int -> Int -> M.Map Name Int -> AnnotAlter (S.Set Name) Name -> AnnotAlter Int (Annot Int Name)
deBruijnAlter k k' env (AlterF tag xs body) = AlterF tag xs' body'
  where xs' = map (Annot . (,) k') xs
        body' = deBruijnExpr k env' body
        env' = extend env (zip xs (repeat k'))

deBruijnFreeVars :: M.Map Name Int -> S.Set Name -> Int
deBruijnFreeVars env fv = foldr max 0 xs
  where xs = [M.findWithDefault 0 v env | v <- S.toList fv]
