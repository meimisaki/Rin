{-# LANGUAGE PatternSynonyms #-}

module Core.AST
( Expr (..)
, pattern ECond
, Alter
, Supercomb
, Program
) where

import Common

data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int -- tag, arity
  | EAp (Expr a) (Expr a)
  | ELet Bool [(a, Expr a)] (Expr a)
  | ECase (Expr a) [Alter a]
  | EAbs [a] (Expr a)
  deriving Show -- TODO: pretty print

pattern ECond e0 e1 e2 = EAp (EAp (EAp (EVar "if") e0) e1) e2

type Alter a = (Int, [a], Expr a)

type Supercomb a = (Name, [a], Expr a)

type Program a = [Supercomb a]
