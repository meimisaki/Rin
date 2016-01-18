{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.AnnotAST where

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
  deriving Functor

deriving instance (Show a, Show (ExprF f a)) => Show (ExprF' f a)

data AlterF f a = AlterF Int [a] (ExprF f a)
  deriving Functor

deriving instance (Show a, Show (ExprF f a)) => Show (AlterF f a)

data SupercombF f a = SupercombF Name [a] (ExprF f a)
  deriving Functor

deriving instance (Show a, Show (ExprF f a)) => Show (SupercombF f a)

newtype ProgramF f a = ProgramF { getProgramF :: [SupercombF f a] }
  deriving (Functor, Monoid)

deriving instance (Show a, Show (ExprF f a)) => Show (ProgramF f a)

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
toAlter f (AlterF tag xs body) = Alter tag xs (toExpr f body)

newtype Annot a b = Annot { runAnnot :: (a, b) }
  deriving (Show, Functor)

getAnnot :: Annot a b -> a
getAnnot = fst . runAnnot

unAnnot :: Annot a b -> b
unAnnot = snd . runAnnot

type AnnotExpr a b = ExprF (Annot a) b

removeAnnot :: AnnotExpr a b -> Expr b
removeAnnot = toExpr unAnnot

type AnnotAlter a b = AlterF (Annot a) b

type AnnotSupercomb a b = SupercombF (Annot a) b

type AnnotProgram a b = ProgramF (Annot a) b
