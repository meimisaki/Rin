module Parser.PostProcess
( PP, runPP
, PPEnv (..), extendOpEnv, lookupOpEnv
, process
) where

import Common

import Control.Monad.Except
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
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

data PPEnv = PPEnv { opEnv :: M.Map Name Fixity }

extendOpEnv :: Name -> Fixity -> PP a -> PP a
extendOpEnv op fix pp = do
  env <- get
  put env { opEnv = M.insert op fix (opEnv env) }
  pp <* put env

lookupOpEnv :: Name -> PP Fixity
lookupOpEnv op = do
  env <- gets opEnv
  return (M.findWithDefault defaultFixity op env)

process :: [Dec] -> PP [Dec]
process decs = foldr go cont decs
  where go dec cont = case dec of
          InfixD fix ops -> foldr (flip extendOpEnv fix) cont ops
          _ -> cont
        cont = mapM pdec (group decs)

group :: [Dec] -> [Dec]
group decs = map collect (L.groupBy eq decs)
  where collect [dec] = dec
        collect decs@(FunD var _:_) = FunD var xs
          where xs = concatMap (\(FunD _ xs) -> xs) decs
        eq (FunD var1 _) (FunD var2 _) = var1 == var2
        eq _ _ = False

data Op = Op Name Fixity

instance Pretty Op where
  pprint (Op op fix) = pprint (InfixD fix [op])

resolve :: [Either Op Exp] -> PP Exp
resolve = fmap fst . parse op1
  where op1 = Op "" (Fixity InfixN (minPrec - 1))
        parse _ [Right e] = return (e, [])
        parse op1 (Right e1:Left op2:xs)
          | prec1 == prec2 && (fix1 /= fix2 || fix1 == InfixN) = throwPP $ sep
            [ text "Cannot mix"
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
  ValD pat body decs -> ValD pat <$> pbody body <*> process decs
  _ -> return dec

pclause :: Clause -> PP Clause
pclause (pats, body, decs) = (,,) pats <$> pbody body <*> process decs

pbody :: Body -> PP Body
pbody (NormalB e) = NormalB <$> pexp e
pbody (GuardedB xs) = GuardedB <$> mapM pguard xs
  where pguard (g, e) = (,) <$> pexp g <*> pexp e

pexp :: Exp -> PP Exp
pexp e = case e of
  AppE e1 e2 -> AppE <$> pexp e1 <*> pexp e2
  GInfixE e1 op e2 -> GInfixE <$> pmaybe e1 <*> return op <*> pmaybe e2
    where pmaybe Nothing = return Nothing
          pmaybe (Just e) = Just <$> pexp e
  UInfixE e1 op e2 -> go e >>= resolve
    where go e = case e of
            UInfixE e1 op e2 -> do
              xs <- go e1
              fix <- lookupOpEnv op
              e3 <- pexp e2
              return (xs ++ [Left (Op op fix), Right e3])
            _ -> return . Right <$> pexp e
  ParensE e -> ParensE <$> pexp e
  LamE pats e -> LamE pats <$> pexp e
  TupE exps -> TupE <$> mapM pexp exps
  ListE exps -> ListE <$> mapM pexp exps
  CondE e0 e1 e2 -> CondE <$> pexp e0 <*> pexp e1 <*> pexp e2
  CaseE e alts -> CaseE <$> pexp e <*> mapM pmatch alts
  LetE decs e -> LetE <$> process decs <*> pexp e
  _ -> return e

pmatch :: Match -> PP Match
pmatch (pat, body, decs) = (,,) pat <$> pbody body <*> process decs
