{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.AST
( Expr (..)
, isAtomic
, Alter (..)
, Supercomb (..)
, Program (..)
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
  deriving (Show, Functor)

isAtomic :: Expr a -> Bool
isAtomic e = case e of
  EVar _ -> True
  ENum _ -> True
  _ -> False

data Alter a = Alter Int [a] (Expr a)
  deriving (Show, Functor)

data Supercomb a = Supercomb Name [a] (Expr a)
  deriving (Show, Functor)

newtype Program a = Program { getProgram :: [Supercomb a] }
  deriving (Show, Functor, Monoid)
