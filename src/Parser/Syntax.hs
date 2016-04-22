{-# LANGUAGE PatternSynonyms #-}

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

type Clause = ([Pat], Body, [Dec])

data Body
  = NormalB Exp
  | GuardedB [(Guard, Exp)]

type Guard = Exp

data Exp
  = VarE Name
  | ConE Name
  | LitE Lit
  | AppE Exp Exp
  | GInfixE (Maybe Exp) Name (Maybe Exp)
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

pattern LSecE e op = GInfixE (Just e) op Nothing
pattern RSecE op e = GInfixE Nothing op (Just e)
pattern InfixE e1 op e2 = GInfixE (Just e1) op (Just e2)

type Pat = Exp

type Match = (Pat, Body, [Dec])

data Lit
  = CharL Char
  | StringL String
  | NumberL Int

data Con = NormalC Name [Type]

pattern InfixC ty1 op ty2 = NormalC op [ty1, ty2]

data Fixity = Fixity Assoc Int

data Assoc = InfixN | InfixL | InfixR
  deriving Eq

isWired :: Name -> Bool
isWired x = case x of
  ":" -> True
  ch:_ -> ch == '(' || ch == '['
  _ -> False

isVarId :: Name -> Bool
isVarId x = case x of
  '_':_:_ -> True
  ch:_ -> isLower ch
  _ -> False

isConId :: Name -> Bool
isConId x = case x of
  ch:_ -> isUpper ch
  _ -> False

isVarSym :: Name -> Bool
isVarSym x = case x of
  ch:_ -> not (isAlpha ch) && ch /= '_' && ch /= ':'
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

binders :: [Dec] -> [Name]
binders decs = go decs []
  where go [] = id
        go (dec:decs) = go decs . case dec of
          FunD var _ -> (var:)
          ValD pat _ _ -> bpat pat
          DataD _ _ dcs -> compose bcon dcs
          _ -> id

arguments :: [Pat] -> [Name]
arguments pats = compose bpat pats []

bpat :: Pat -> [Name] -> [Name]
bpat pat = case pat of
  VarE var -> (var:)
  AppE pat1 pat2 -> bpat pat1 . bpat pat2
  InfixE pat1 _ pat2 -> bpat pat1 . bpat pat2
  UInfixE pat1 op pat2 -> bpat (InfixE pat1 op pat2)
  ParensE pat -> bpat pat
  TupE pats -> compose bpat pats
  ListE pats -> compose bpat pats
  AsE var pat -> (var:) . bpat pat
  _ -> id

bcon :: Con -> [Name] -> [Name]
bcon (NormalC dc _) = (dc:)

operators :: [Dec] -> [(Name, Fixity)]
operators decs = go decs []
  where go [] = id
        go (dec:decs) = go decs . case dec of
          InfixD fix ops -> (zip ops (repeat fix) ++)
          _ -> id
