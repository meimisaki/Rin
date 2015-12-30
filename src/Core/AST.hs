module Core.AST
( Expr (..)
, isAtomic
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
  deriving Show

isAtomic :: Expr a -> Bool
isAtomic e = case e of
  EVar _ -> True
  ENum _ -> True
  _ -> False

type Alter a = (Int, [a], Expr a)

type Supercomb a = (Name, [a], Expr a)

type Program a = [Supercomb a]
