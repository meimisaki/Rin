{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Parser.PostProcess
( PP, runPP, throwPP
, PPEnv (..)
, extendOpEnv, lookupOpEnv
, extendVarEnv, lookupVarEnv
, process
) where

import Common

import Control.Monad.Except
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Parser.PrettyPrint
import Parser.Syntax
import Text.PrettyPrint

-- maybe we should use `Reader` rather than `State`
-- and create a new environment whenever we extend it
type PP a = ExceptT String (State PPEnv) a

runPP :: PP a -> PPEnv -> (Either String a, PPEnv)
runPP = runState . runExceptT

throwPP :: Show a => a -> PP b
throwPP = throwError . show

data PPEnv = PPEnv
  { opEnv :: M.Map Name Fixity
  , varEnv :: S.Set Name }

extendOpEnv :: Name -> Fixity -> PP a -> PP a
extendOpEnv op fix pp = do
  env <- get
  put env { opEnv = M.insert op fix (opEnv env) }
  pp <* put env

lookupOpEnv :: Name -> PP Fixity
lookupOpEnv op = do
  env <- gets opEnv
  return (M.findWithDefault defaultFixity op env)

extendVarEnv :: Name -> PP a -> PP a
extendVarEnv var pp = do
  env <- get
  put env { varEnv = S.insert var (varEnv env) }
  pp <* put env

lookupVarEnv :: Name -> PP Bool
lookupVarEnv var = do
  env <- gets varEnv
  return (S.member var env)

process :: [Dec] -> PP [Dec]
process decs = pdecs decs return

pdecs :: [Dec] -> ([Dec] -> PP a) -> PP a
pdecs (group -> decs) cont = do
  checkEquations decs
  bindOps vars ops (bindVars vars (mapM pdec decs >>= cont))
  where vars = binders decs
        ops = operators decs

checkEquations :: [Dec] -> PP ()
checkEquations decs = case first diffArgs decs of
  Nothing -> return ()
  Just var -> throwPP $ sep
    [ text "Equations for"
    , pid var
    , text "have different number of arguments" ]
  where diffArgs (FunD var xs) | any (/= a) as = Just var
          where a:as = [length pats | (pats, _, _) <- xs]
        diffArgs _ = Nothing

checkConflicts :: [Name] -> (Maybe Name -> PP a) -> PP a
checkConflicts vars cont = cont (first conflict (M.assocs bndrs))
  where bndrs = M.fromListWith (+) [(var, 1) | var <- vars]
        conflict (var, n)
          | n <= 1 = Nothing
          | otherwise = Just var

bindVars :: [Name] -> PP a -> PP a
bindVars vars cont = checkConflicts vars $ \case
  Nothing -> compose extendVarEnv vars cont
  Just var -> throwPP $ sep
    [ text "Conflicting definitions for"
    , pid var ]

bindOps :: [Name] -> [(Name, Fixity)] -> PP a -> PP a
bindOps vars ops cont = case first noBind names of
  Nothing -> checkConflicts names $ \case
    Nothing -> compose (uncurry extendOpEnv) ops cont
    Just op -> throwPP $ sep
      [ text "Multiple fixity declarations for"
      , psym op ]
  Just op -> throwPP $ sep
    [ text "Fixity declaration for"
    , psym op
    , text "lacks an accompanying binding" ]
  where names = map fst ops
        bndrs = S.fromList vars
        noBind op
          | S.member op bndrs = Nothing
          | otherwise = Just op

-- TODO: check variable in scope

group :: [Dec] -> [Dec]
group decs = map collect (L.groupBy eq decs)
  where collect [dec] = dec
        collect decs@(FunD var _:_) = FunD var xs
          where xs = concatMap (\(FunD _ xs) -> xs) decs
        eq (FunD var1 _) (FunD var2 _) = var1 == var2
        eq _ _ = False

data Op = Op Name Fixity

instance Pretty Op where
  pprint (Op op fix) = psym op <+> brackets (pprint fix)

resolve :: [Either Op Exp] -> PP Exp
resolve = fmap fst . parse op1
  where op1 = Op "" (Fixity InfixN (minPrec - 1))
        parse _ [Right e] = return (e, [])
        parse op1 (Right e1:Left op2:xs)
          | prec1 == prec2 && (fix1 /= fix2 || fix1 == InfixN) = throwPP $ sep
            [ text "Precedence parsing error:"
            , text "cannot mix"
            , pprint op1
            , text "and"
            , pprint op2
            , text "in the same infix expression" ]
          | prec1 > prec2 || (prec1 == prec2 && fix1 == InfixL) = return (e1, Left op2:xs)
          | otherwise = do
            (e2, xs1) <- parse op2 xs
            parse op1 (Right (InfixE e1 op e2):xs1)
          where Op _ (Fixity fix1 prec1) = op1
                Op op (Fixity fix2 prec2) = op2

pdec :: Dec -> PP Dec
pdec dec = case dec of
  FunD var xs -> FunD var <$> mapM pclause xs
  ValD pat body decs -> do
    pat1 <- pexp pat
    pdecs decs $ \decs1 -> do
      body1 <- pbody body
      return (ValD pat1 body1 decs1)
  _ -> return dec

pclause :: Clause -> PP Clause
pclause (pats, body, decs) = bindVars args $ do
  pats1 <- mapM pexp pats
  pdecs decs $ \decs1 -> do
    body1 <- pbody body
    return (pats1, body1, decs1)
  where args = arguments pats

pbody :: Body -> PP Body
pbody (NormalB e) = NormalB <$> pexp e
pbody (GuardedB xs) = GuardedB <$> mapM pguard xs
  where pguard (g, e) = (,) <$> pexp g <*> pexp e

checkBinds :: Name -> PP Name
checkBinds var
  | isWired var = return var
  | otherwise = lookupVarEnv var >>= \bound -> if bound
    then return var
    else throwPP $ sep
      [ text "Not in scope:"
      , pid var ]

pexp :: Exp -> PP Exp
pexp e = case e of
  VarE var -> VarE <$> checkBinds var
  ConE con -> ConE <$> checkBinds con
  AppE e1 e2 -> AppE <$> pexp e1 <*> pexp e2
  GInfixE e1 op e2 -> GInfixE <$> pmaybe e1 <*> return op <*> pmaybe e2
    where pmaybe Nothing = return Nothing
          pmaybe (Just e) = Just <$> pexp e
  UInfixE e1 op e2 -> go e >>= resolve . reverse
    where go e = case e of
            UInfixE e1 op e2 -> do
              xs <- go e1
              fix <- lookupOpEnv op
              e3 <- pexp e2
              return (Right e3:Left (Op op fix):xs)
            _ -> return . Right <$> pexp e
  ParensE e -> ParensE <$> pexp e
  LamE pats e -> bindVars args (LamE <$> mapM pexp pats <*> pexp e)
    where args = arguments pats
  TupE exps -> TupE <$> mapM pexp exps
  ListE exps -> ListE <$> mapM pexp exps
  CondE e0 e1 e2 -> CondE <$> pexp e0 <*> pexp e1 <*> pexp e2
  CaseE e alts -> CaseE <$> pexp e <*> mapM pmatch alts
  LetE decs e -> pdecs decs $ \decs1 -> LetE decs1 <$> pexp e
  _ -> return e

pmatch :: Match -> PP Match
pmatch (pat, body, decs) = bindVars args $ do
  pat1 <- pexp pat
  pdecs decs $ \decs1 -> do
    body1 <- pbody body
    return (pat1, body1, decs1)
  where args = arguments [pat]
