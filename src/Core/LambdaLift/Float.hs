module Core.LambdaLift.Float
( float
) where

import Common

import Core.AST
import Core.AnnotAST
import Data.List

float :: Program (Annot Int Name) -> Program Name
float = Program . concatMap floatSC . getProgram
  where floatSC (Supercomb name [] body) = Supercomb name [] body':scDefs
          where (fd, body') = floatExpr body
                scDefs = [Supercomb x [] e | (_, _, defs) <- fd, (x, e) <- defs]

type Floated = [(Int, Bool, [(Name, Expr Name)])]

-- combine directly nested lambdas
mkAbs :: [Name] -> Expr Name -> Expr Name
mkAbs args1 (EAbs args2 body) = EAbs (args1 ++ args2) body
mkAbs args body = EAbs args body

floatExpr :: Expr (Annot Int Name) -> (Floated, Expr Name)
floatExpr e = case e of
  EVar v -> ([], EVar v)
  ENum n -> ([], ENum n)
  EConstr tag arity -> ([], EConstr tag arity)
  EAp e1 e2 -> (fd1 ++ fd2, EAp e1' e2')
    where (fd1, e1') = floatExpr e1
          (fd2, e2') = floatExpr e2
  ELet rec defs body -> (defsFD ++ (k, rec, defs'):bodyFD, body')
    where (defsFD, defs') = mapAccumR go [] defs
          (bodyFD, body') = floatExpr body
          k = getAnnot (fst (head defs))
          go fd (x, e) = (fd' ++ fd, (unAnnot x, e'))
            where (fd', e') = floatExpr e
  ECase e alts -> (fd ++ altsFD, ECase e' alts')
    where (fd, e') = floatExpr e
          (altsFD, alts') = accum (map floatAlter alts)
  EAbs args body -> (outer, mkAbs args' (install inner body'))
    where args' = map unAnnot args
          (fd, body') = floatExpr body
          (outer, inner) = partitionFD k fd
          k = getAnnot (head args)

floatAlter :: Alter (Annot Int Name) -> (Floated, Alter Name)
floatAlter (Alter tag xs body) = (outer, Alter tag xs' (install inner body'))
  where xs' = map unAnnot xs
        (fd, body') = floatExpr body
        (outer, inner) = partitionFD k fd
        k = head (map getAnnot xs ++ [maxBound])

partitionFD :: Int -> Floated -> (Floated, Floated)
partitionFD k = partition (\(k', _, _) -> k' < k)

install :: Floated -> Expr Name -> Expr Name
install fd e = foldr (\(_, rec, defs) -> ELet rec defs) e fd
