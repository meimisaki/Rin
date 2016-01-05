module Core.LambdaLift.MFE
( identifyMFE
) where

import Common

import Core.AST
import Core.AnnotAST
import Core.Prelude

identifyMFE :: AnnotProgram Int (Annot Int Name) -> Program (Annot Int Name)
identifyMFE = Program . map identifySC . getProgramF
  where identifySC (SupercombF name [] body) = Supercomb name [] body'
          where body' = identifyExpr 0 body

transformMFE :: Int -> Expr (Annot Int Name) -> Expr (Annot Int Name)
transformMFE k e = ELet False [(Annot (k, anonym), e)] (EVar anonym)

-- check whether a redex
notCandidate :: Expr (Annot Int Name) -> Bool
notCandidate e = case e of
  EVar _ -> True
  ENum _ -> True
  EConstr _ _ -> True
  EAp (EVar v) _ -> elem v operators
  _ -> False

identifyExpr :: Int -> AnnotExpr Int (Annot Int Name) -> Expr (Annot Int Name)
identifyExpr cxt a@(Annot (k, e))
  | cxt == k || notCandidate e' = e'
  | otherwise = transformMFE k e'
  where e' = identifyExpr1 a

identifyExpr1 :: AnnotExpr Int (Annot Int Name) -> Expr (Annot Int Name)
identifyExpr1 (Annot (k, e)) = case e of
  EVarF v -> EVar v
  ENumF n -> ENum n
  EConstrF tag arity -> EConstr tag arity
  EApF e1 e2 -> EAp (identifyExpr k e1) (identifyExpr k e2)
  ELetF rec defs body -> ELet rec defs' body'
    where defs' = [(Annot (k, x), identifyExpr k e) | (Annot (k, x), e) <- defs]
          body' = identifyExpr k body
  ECaseF e alts -> ECase (identifyExpr k e) (map (identifyAlter k) alts)
  EAbsF args body -> EAbs args (identifyExpr k' body)
    where k' = getAnnot (head args)

identifyAlter :: Int -> AnnotAlter Int (Annot Int Name) -> Alter (Annot Int Name)
identifyAlter k (AlterF tag xs body) = Alter tag xs (identifyExpr k body)
