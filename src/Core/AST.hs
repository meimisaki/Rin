module Core.AST
( Expr (..)
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
  | ELet [(a, Expr a)] (Expr a)
  | EFix [(a, Expr a)] (Expr a)
  | ECase (Expr a) [Alter a]
  | EAbs [a] (Expr a)
  deriving Show -- TODO: pretty print

type Alter a = (Int, [a], Expr a)

type Supercomb a = (Name, [a], Expr a)

type Program a = [Supercomb a]
