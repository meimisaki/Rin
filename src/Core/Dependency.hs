{-# LANGUAGE ViewPatterns #-}

module Core.Dependency
( dependency
) where

import Common

import Core.AST
import Core.AnnotAST
import Core.FreeVars
import Data.Graph
import qualified Data.Set as S

dependency :: Program Name -> Program Name
dependency = depend . freeVars

depend :: AnnotProgram (S.Set Name) Name -> Program Name
depend = Program . map dependSC . getProgramF
  where dependSC (SupercombF name args body) = Supercomb name args (dependExpr body)

dependExpr :: AnnotExpr (S.Set Name) Name -> Expr Name
dependExpr (unAnnot -> e) = case e of
  EVarF v -> EVar v
  ENumF n -> ENum n
  EConstrF tag arity -> EConstr tag arity
  EApF e1 e2 -> EAp (dependExpr e1) (dependExpr e2)
  ELetF rec defs body -> foldr mkLet (dependExpr body) comps
    where graph = [(e, x, link fv) | (x, e@(getAnnot -> fv)) <- defs]
          comps = stronglyConnCompR graph
          link fv = if rec then S.toList (S.intersection fv (S.fromList xs)) else []
          xs = map fst defs
  ECaseF e alts -> ECase (dependExpr e) (map dependAlter alts)
  EAbsF args body -> EAbs args (dependExpr body)

mkLet :: SCC (AnnotExpr (S.Set Name) Name, Name, [Name]) -> Expr Name -> Expr Name
mkLet (AcyclicSCC (e, x, _)) = ELet False [(x, dependExpr e)]
mkLet (CyclicSCC comp) = ELet True [(x, dependExpr e) | (e, x, _) <- comp]

dependAlter :: AnnotAlter (S.Set Name) Name -> Alter Name
dependAlter (AlterF tag xs body) = Alter tag xs (dependExpr body)
