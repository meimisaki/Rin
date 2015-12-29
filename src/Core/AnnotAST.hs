{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.AnnotAST
( ExprF
, ExprF' (..)
, AlterF
, SupercombF
, ProgramF
, toExpr
, toAlter
, Annot (..)
, AnnotExpr
, removeAnnot
) where

import Common

import Core.AST

type ExprF f a = f (ExprF' f a)

data ExprF' f a
  = EVarF Name
  | ENumF Int
  | EConstrF Int Int -- tag, arity
  | EApF (ExprF f a) (ExprF f a)
  | ELetF Bool [(a, ExprF f a)] (ExprF f a)
  | ECaseF (ExprF f a) [AlterF f a]
  | EAbsF [a] (ExprF f a)

type AlterF f a = (Int, [a], ExprF f a)

type SupercombF f a = (Name, [a], ExprF f a)

type ProgramF f a = [SupercombF f a]

deriving instance (Show a, Show (ExprF f a)) => Show (ExprF' f a)

toExpr :: (ExprF f a -> ExprF' f a) -> ExprF f a -> Expr a
toExpr f e = case f e of
  EVarF v -> EVar v
  ENumF n -> ENum n
  EConstrF tag arity -> EConstr tag arity
  EApF e1 e2 -> EAp (toExpr f e1) (toExpr f e2)
  ELetF rec defs body -> ELet rec (zip xs exps) (toExpr f body)
    where xs = map fst defs
          exps = map (toExpr f . snd) defs
  ECaseF e alts -> ECase (toExpr f e) (map (toAlter f) alts)
  EAbsF args body -> EAbs args (toExpr f body)

toAlter :: (ExprF f a -> ExprF' f a) -> AlterF f a -> Alter a
toAlter f (tag, xs, body) = (tag, xs, toExpr f body)

newtype Annot a b = Annot { unAnnot :: (a, b) } deriving Show

type AnnotExpr a b = ExprF (Annot a) b

removeAnnot :: AnnotExpr a b -> Expr b
removeAnnot = toExpr (snd . unAnnot)
