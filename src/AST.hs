{-# LANGUAGE PatternSynonyms #-}

module AST
( Exp (..)
, pattern ELet
, EVar
, Literal (..)
, Type (..)
, pattern TArr
, TVar
, TCon
) where

import Data.Char

data Exp
  = EVar EVar
  | ELit Literal
  | EApp Exp Exp
  | EAbs EVar Exp
  | EFix [(EVar, Exp)] Exp
  deriving Eq

instance Show Exp where
  show (EVar x) = x
  show (ELit x) = show x
  show (EApp e1 e2) = show e1 ++ " " ++ paren e2
    where paren e@(EApp _ _) = "(" ++ show e ++ ")"
          paren e@(EAbs _ _) = "(" ++ show e ++ ")"
          paren e@(EFix _ _) = "(" ++ show e ++ ")"
          paren e = show e
  show (EAbs x e) = "\\" ++ x ++ " -> " ++ show e
  show (EFix xs e) = "let " ++ unwords (map eqn xs) ++ " in " ++ show e
    where eqn (x, e) = "(" ++ x ++ " = " ++ show e ++ ")"

pattern ELet x e1 e2 = EFix [(x, e1)] e2

type EVar = String

data Literal
  = LBool Bool
  | LInt Int
  deriving Eq

instance Show Literal where
  show (LBool a) = show a
  show (LInt a) = show a

data Type
  = TVar TVar
  | TCon TCon
  | TApp Type Type
  | TForall [TVar] Type
  deriving Eq

pattern TArr t1 t2 = TApp (TApp (TCon "->") t1) t2

instance Show Type where
  show (TVar x) = x
  show (TCon x) = if all isAlphaNum x then x else "(" ++ x ++ ")"
  show (TArr t1 t2) = paren t1 ++ " -> " ++ show t2
    where paren t@(TArr _ _) = "(" ++ show t ++ ")"
          paren t = show t
  show (TApp t1 t2) = show t1 ++ " " ++ paren t2
    where paren t@(TApp _ _) = "(" ++ show t ++ ")"
          paren t = show t
  show (TForall vars t) = "forall " ++ unwords vars ++ ". " ++ show t

type TVar = String

type TCon = String
