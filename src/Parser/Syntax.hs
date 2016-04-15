module Parser.Syntax
( module Parser.Syntax
, module Type.Types
) where

import Common

import Data.Char
import Type.PrettyPrint
import Type.Types

data Dec
  = FunD Name [Clause]
  | ValD Pat Body [Dec]
  | DataD Name [Name] [Con]
  | SigD [Name] Type
  | InfixD Fixity [Name]
  deriving Show

type Clause = ([Pat], Body, [Dec])

data Body
  = NormalB Exp
  | GuardedB [(Guard, Exp)]
  deriving Show

type Guard = Exp

data Exp
  = VarE Name
  | ConE Name
  | LitE Lit
  | AppE Exp Exp
  | LSecE Exp Name
  | RSecE Name Exp
  | InfixE Exp Name Exp
  | UInfixE Exp Name Exp
  | ParensE Exp -- necessary, since we leave infix expression unresolved
  | LamE [Pat] Exp
  | TupE [Exp]
  | ListE [Exp]
  | CondE Exp Exp Exp
  | CaseE Exp [Match]
  | LetE [Dec] Exp
  | SigE Exp Type
  -- for patterns
  | AsE Name Pat
  | WildE
-- TODO: arith sequences, list comprehensions, etc.
  deriving Show

type Pat = Exp

type Match = (Pat, Body, [Dec])

data Lit
  = CharL Char
  | StringL String
  | NumberL Int
  deriving Show

data Con
  = NormalC Name [Type]
  | InfixC Type Name Type
  deriving Show

data Fixity = Fixity Assoc Int
  deriving Show

data Assoc = InfixN | InfixL | InfixR
  deriving Show

isVarId :: Name -> Bool
isVarId x = case x of
  ch:_ | isLower ch || ch == '_' -> True
  _ -> False

isConId :: Name -> Bool
isConId x = case x of
  ch:_ | isUpper ch -> True
  _ -> False

isVarSym :: Name -> Bool
isVarSym x = case x of
  ch:_ | ch /= ':' -> True
  _ -> False

isConSym :: Name -> Bool
isConSym x = case x of
  ':':_ -> True
  _ -> False

minPrec :: Int
minPrec = 0

maxPrec :: Int
maxPrec = 9

defaultAssoc :: Assoc
defaultAssoc = InfixL

defaultPrec :: Int
defaultPrec = maxPrec

defaultFixity :: Fixity
defaultFixity = Fixity defaultAssoc defaultPrec

tupCon :: Int -> Name
tupCon n = "(" ++ replicate n ',' ++ ")"

listCon :: Name
listCon = "[]"
