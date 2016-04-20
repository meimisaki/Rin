module Core.Rename
( rename
, rename_
) where

import Common

import Core.AST
import Data.NameSupply
import Data.Functor.Identity
import qualified Data.Set as S

rename_ :: Program Name -> Program Name
rename_ = fmap runIdentity . rename runIdentity . fmap Identity

rename :: Functor f => (f Name -> Name) -> Program (f Name) -> Program (f Name)
rename nameOf (Program prog) = Program (fst (runNS (mapM renameSC prog) supply))
  where supply = mkNameSupply (S.fromList [name | (Supercomb name _ _) <- prog])
        renameSC (Supercomb name args body) = renameArgs nameOf args cont
          where cont args = Supercomb name args <$> renameExpr nameOf body

renameArgs :: Functor f => (f Name -> Name) -> [f Name] -> ([f Name] -> NS a) -> NS a
renameArgs nameOf args cont = do
  let names = map nameOf args
  names1 <- mapM (const newName) names
  let args1 = map (\(arg, name) -> fmap (const name) arg) (zip args names1)
  compose (uncurry withName) (zip names names1) (cont args1)

renameExpr :: Functor f => (f Name -> Name) -> Expr (f Name) -> NS (Expr (f Name))
renameExpr nameOf e = case e of
  EVar v -> EVar <$> findName v
  ENum _ -> return e
  EConstr _ _ -> return e
  EAp e1 e2 -> EAp <$> renameExpr nameOf e1 <*> renameExpr nameOf e2
  ELet rec defs body -> if rec
    then rxs $ \xs -> rexps >>= cont xs
    else rexps >>= rxs . flip cont
    where cont xs exps = ELet rec (zip xs exps) <$> renameExpr nameOf body
          rxs = renameArgs nameOf xs
          rexps = mapM (renameExpr nameOf) exps
          (xs, exps) = unzip defs
  ECase e alts -> ECase <$> renameExpr nameOf e <*> mapM (renameAlter nameOf) alts
  EAbs args body -> renameArgs nameOf args cont
    where cont args = EAbs args <$> renameExpr nameOf body

renameAlter :: Functor f => (f Name -> Name) -> Alter (f Name) -> NS (Alter (f Name))
renameAlter nameOf (Alter tag xs body) = renameArgs nameOf xs cont
  where cont xs = Alter tag xs <$> renameExpr nameOf body
