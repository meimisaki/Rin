{
module Parser.Parser
( parse
) where

import Common

import Parser.Lexer
import Parser.Syntax
}

-- TODO: better diagnostic

%expect 6

%token
 'data' { L _ ITdata } -- keywords
 'case' { L _ ITcase }
 'of' { L _ ITof }
 'if' { L _ ITif }
 'then' { L _ ITthen }
 'else' { L _ ITelse }
 'let' { L _ ITlet }
 'in' { L _ ITin }
 'where' { L _ ITwhere }
 'infix' { L _ ITinfix }
 'infixl' { L _ ITinfixl }
 'infixr' { L _ ITinfixr }
 'forall' { L _ ITforall }
 '_' { L _ ITunderscore }
 '@' { L _ ITat } -- reserved symbols
 '.' { L _ ITdot }
 '..' { L _ ITddot }
 '::' { L _ ITdcolon }
 '=' { L _ ITequal }
 '\\' { L _ ITlambda }
 '<-' { L _ ITlarrow }
 '->' { L _ ITrarrow }
 '=>' { L _ ITdarrow }
 '|' { L _ ITvbar }
 '{' { L _ ITocurly } -- special symbols
 '}' { L _ ITccurly }
 vocurly { L _ ITvocurly }
 vccurly { L _ ITvccurly }
 '[' { L _ ITobrack }
 ']' { L _ ITcbrack }
 '(' { L _ IToparen }
 ')' { L _ ITcparen }
 ';' { L _ ITsemi }
 ',' { L _ ITcomma }
 '`' { L _ ITbackquote }
 VARID { L _ (ITvarid _) } -- identifiers
 CONID { L _ (ITconid _) }
 VARSYM { L _ (ITvarsym _) }
 CONSYM { L _ (ITconsym _) }
 CHAR { L _ (ITchar _) } -- literals
 STRING { L _ (ITstring _) }
 NUMBER { L _ (ITnumber _) }

%name parse program

%monad { P } { >>= } { return }
%lexer { lexer } { L _ ITeof }
%tokentype { Located Token }

%%

program :: { [Dec] }
  : '{' topdecls '}' { $2 }
  | open topdecls close { $2 }

var :: { Name }
  : VARID { getVARID $1 }
  | '(' VARSYM ')' { getVARSYM $2 }
  | '(' '.' ')' { "." }

con :: { Name }
  : CONID { getCONID $1 }
  | '(' CONSYM ')' { getCONSYM $2 }
  | wired { $1 }

wired :: { Name }
  : '(' ')' { tupCon 0 }
  | '(' commas ')' { tupCon $2 }
  | '[' ']' { listCon }

varop :: { Name }
  : VARSYM { getVARSYM $1 }
  | '`' VARID '`' { getVARID $2 }
  | '.' { "." }

conop :: { Name }
  : CONSYM { getCONSYM $1 }
  | '`' CONID '`' { getCONID $2 }

op :: { Name }
  : varop { $1 }
  | conop { $1 }

commas :: { Int }
  : commas ',' { $1 + 1 }
  | ',' { 1 }

vars :: { [Name] }
  : vars ',' var { $1 ++ [$3] }
  | var { [$1] }

ops :: { [Name] }
  : ops ',' op { $1 ++ [$3] }
  | op { [$1] }

tyvar :: { Name }
  : VARID { getVARID $1 }

tycon :: { Name }
  : CONID { getCONID $1 }
  | wired { $1 }

tyvars :: { [Name] }
  : tyvars tyvar { $1 ++ [$2] }
  | {- empty -} { [] }

open :: { () }
  : {- empty -} {% pushCurrentContext }

close :: { () }
  : vccurly { () }
  | error {% popContext }

topdecls :: { [Dec] }
  : topdecls ';' topdecl { $1 ++ [$3] }
  | topdecls ';' { $1 }
  | topdecl { [$1] }

topdecl :: { Dec }
  : fundecl { $1 }
  | datadecl { $1 }
  | sigdecl { $1 }
  | fixitydecl { $1 }

fundecl :: { Dec }
  : infixexp rhs wherebinds {% checkFun $1 $2 $3 }

rhs :: { Body }
  : '=' exp { NormalB $2 }
  | gdrhss { GuardedB $1 }

gdrhss :: { [(Guard, Exp)] }
  : gdrhss gdrhs { $1 ++ [$2] }
  | gdrhs { [$1] }

gdrhs :: { (Guard, Exp) }
  : '|' exp '=' exp { ($2, $4) }

datadecl :: { Dec }
  : 'data' tycon tyvars '=' constrs { DataD $2 $3 $5 }

constrs :: { [Con] }
  : constrs '|' constr { $1 ++ [$3] }
  | constr { [$1] }

constr :: { Con }
  : btype {% checkDataCon $1 }
  | btype conop btype { InfixC $1 $2 $3 }

sigdecl :: { Dec }
  : vars '::' type { SigD $1 $3 }

fixitydecl :: { Dec }
 : assoc prec ops { InfixD (Fixity $1 $2) $3 }

assoc :: { Assoc }
  : 'infix' { InfixN }
  | 'infixl' { InfixL }
  | 'infixr' { InfixR }

prec :: { Int }
  : NUMBER {% checkPrec (getNUMBER $1) }
  | {- empty -} { defaultPrec }

type :: { Type }
  : btype '->' ctype { TyArr $1 $3 }
  | btype { $1 }

ctype :: { Type }
  : 'forall' tyvars '.' ctype { mkForall (map Bound $2) $4 }
  | type { $1 }

btype :: { Type }
  : btype atype { TyAp $1 $2 } -- TODO: check predicativity
  | atype { $1 }

atype :: { Type }
  : tyvar { TyVar (Bound $1) }
  | tycon { TyCon $1 }
  | '(' ctype ')' { $2 }
  | '(' types ')' {
    let tc = TyCon (tupCon (length $2 - 1))
    in foldl TyAp tc $2 }
  | '[' type ']' { TyAp (TyCon listCon) $2 } -- TODO: check predicativity

types :: { [Type] }
  : types ',' type { $1 ++ [$3] }
  | type ',' type { [$1, $3] }

exp :: { Exp }
  : infixexp '::' type { SigE $1 $3 }
  | infixexp { $1 }

infixexp :: { Exp }
  : infixexp op exp1 { UInfixE $1 $2 $3 }
  | exp1 { $1 }

exp1 :: { Exp }
  : '\\' apats '->' exp { LamE $2 $4 }
  | 'let' binds 'in' exp { LetE $2 $4 }
  | 'if' exp 'then' exp 'else' exp { CondE $2 $4 $6 }
  | 'case' exp 'of' cases { CaseE $2 $4 }
  | fexp { $1 }

fexp :: { Exp }
  : fexp aexp { AppE $1 $2 }
  | aexp { $1 }

aexp :: { Exp }
  : var '@' apat { AsE $1 $3 }
  | '_' { WildE }
  | var { VarE $1 }
  | con { ConE $1 }
  | lit { LitE $1 }
  | '(' texp ')' { ParensE $2 }
  | '(' texps ')' { TupE $2 }
  | '[' texps ']' { ListE $2 }

texp :: { Exp }
  : infixexp op { LSecE $1 $2 }
  | op infixexp { RSecE $1 $2 }
  | exp { $1 }

texps :: { [Exp] }
  : texps ',' exp {% do
    checkSec $3
    return ($1 ++ [$3]) }
  | exp ',' exp {% do
    checkSec $1
    checkSec $3
    return [$1, $3] }

lit :: { Lit }
  : NUMBER { NumberL (getNUMBER $1) }

cases :: { [Match] }
  : '{' alts '}' { $2 }
  | vocurly alts close { $2 }

alts :: { [Match] }
  : alts ';' alt { $1 ++ [$3] }
  | alts ';' { $1 }
  | alt { [$1] }

alt :: { Match }
  : pat altrhs wherebinds { ($1, $2, $3) }

altrhs :: { Body }
  : '->' exp { NormalB $2 }
  | gdalts { GuardedB $1 }

gdalts :: { [(Guard, Exp)] }
  : gdalts gdalt { $1 ++ [$2] }
  | gdalt { [$1] }

gdalt :: { (Guard, Exp) }
  : '|' exp '->' exp { ($2, $4) }

pat :: { Pat }
  : exp {% do
    checkPat $1
    return $1 }

apats :: { [Pat] }
  : apats apat { $1 ++ [$2] }
  | apat { [$1] }

apat :: { Pat }
  : aexp {% do
    checkPat $1
    return $1 }

wherebinds :: { [Dec] }
  : 'where' binds { $2 }
  | {- empty -} { [] }

binds :: { [Dec] }
  : '{' decls '}' { $2 }
  | vocurly decls close { $2 }

decls :: { [Dec] }
 : decls ';' decl { $1 ++ [$3] }
 | decls ';' { $1 }
 | decl { [$1] }
 | {- empty -} { [] }

decl :: { Dec }
  : fundecl { $1 }
  | sigdecl { $1 }
  | fixitydecl { $1 }

{
happyError :: P a
happyError = fail "Parser error"

getVARID, getCONID, getVARSYM, getCONSYM :: Located Token -> Name
getVARID (L _ (ITvarid x)) = x
getCONID (L _ (ITconid x)) = x
getVARSYM (L _ (ITvarsym x)) = x
getCONSYM (L _ (ITconsym x)) = x

getNUMBER :: Located Token -> Int
getNUMBER (L _ (ITnumber n)) = n

checkFun :: Exp -> Body -> [Dec] -> P Dec
checkFun e body decs = do
  case go e of
    Nothing -> do
      checkPat e
      return (ValD e body decs)
    Just (var, []) -> return (ValD (VarE var) body decs)
    Just (var, pats) -> do
      mapM checkPat pats
      return (FunD var [(pats, body, decs)])
  where go (VarE var) = return (var, [])
        go (AppE e pat) = do
          (var, pats) <- go e
          return (var, pats ++ [pat])
        go (ParensE e) = go e
        go e = do
          (pat1, op, pat2) <- go1 e
          return (op, [pat1, pat2])
        go1 (UInfixE e op pat)
          | isVarSym op = return (e, op, pat)
          | otherwise = do
            (pat1, op1, pat2) <- go1 e
            return (pat1, op1, UInfixE pat2 op pat)
        go1 (ParensE e) = go1 e
        go1 _ = fail "Parse error in function binding"

checkDataCon :: Type -> P Con
checkDataCon (TyCon con) | isConId con = return (NormalC con [])
checkDataCon (TyAp ty1 ty2) = do
  NormalC con tys <- checkDataCon ty1
  return (NormalC con (tys ++ [ty2]))
checkDataCon _ = fail "Parse error in data constructor"

checkPrec :: Int -> P Int
checkPrec n
  | minPrec <= n && n <= maxPrec = return n
  | otherwise = fail "Precedence out of range"

-- TODO: check pattern
checkPat :: Pat -> P ()
checkPat pat = return ()

checkSec :: Exp -> P ()
checkSec e = case e of
  LSecE _ _ -> err
  RSecE _ _ -> err
  _ -> return ()
  where err = fail "Section must be enclosed in parentheses"
}
