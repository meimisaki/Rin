module Parser.Syntax
( module Parser.Syntax
, module Type.Types
) where

import Common

import Type.Types

data Dec
  = FunD Name [Clause]
  | ValD Pat Body [Dec]
  | DataD Name [Name] [Con]
  | SigD [Name] Type
  | InfixD Fixity Name

type Clause = ([Pat], Body, [Dec])

data Body
  = NormalB Exp
  | GuardedB [(Guard, Exp)]

data Guard
  = NormalG Exp
  | PatG [Stmt]

data Stmt
  = BindS Pat Exp
  | LetS [Dec]
  | NoBindS Exp

data Exp
  = VarE Name
  | ConE Name
  | LitE Lit
  | AppE Exp Exp
  | InfixE (Maybe Exp) Name (Maybe Exp)
  | UInfixE Exp Name Exp
  | ParensE Exp -- necessary, since we leave infix expression unresolved
  | LamE [Pat] Exp
  | TupE [Maybe Exp]
  | ListE [Exp]
  | CondE Exp Exp Exp
  | LetE [Dec] Exp
  | CaseE Exp [Match]
  | DoE [Stmt]
  | SigE Exp Type
-- TODO: arith sequences, list comprehensions, etc.

type Match = (Pat, Body, [Dec])

data Lit
  = CharL Char
  | IntL Int
  | FloatL Double
  | StringL String

tupCon :: Int -> Type
tupCon len = TyCon (replicate len ',')

listCon :: Type
listCon = TyCon "[]"

data Con
  = NormalC Name [Type]
  | InfixC Type Name Type

data Pat
  = VarP Name
  | ConP Name [Pat]
  | LitP Lit
  | InfixP Pat Name Pat
  | UInfixP Pat Name Pat
  | ParensP Pat
  | AsP Name Pat
  | TupP [Pat]
  | ListP [Pat]
  | WildP

data Fixity = Fixity Assoc Int

data Assoc = InfixN | InfixL | InfixR

minPrecedence :: Int
minPrecedence = 0

maxPrecedence :: Int
maxPrecedence = 9

defaultFixity :: Fixity
defaultFixity = Fixity InfixL maxPrecedence
