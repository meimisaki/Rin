module Parser.PrettyPrint
( Pretty (..)
) where

import Common

import Parser.Syntax
import Text.PrettyPrint
import Type.PrettyPrint

instance Show Dec where
  show = show . pprint

instance Show Exp where
  show = show . pprint

instance Show Lit where
  show (CharL ch) = show ch
  show (StringL xs) = show xs
  show (NumberL n) = show n

instance Show Con where
  show = show . pprint

instance Show Fixity where
  show = show . pprint

instance Show Assoc where
  show InfixN = "infix"
  show InfixL = "infixl"
  show InfixR = "infixr"

pid :: Name -> Doc
pid x
  | isWired x || isVarId x || isConId x = text x
  | otherwise = parens (text x)

psym :: Name -> Doc
psym x
  | isVarSym x || isConSym x = text x
  | otherwise = bquote <> text x <> bquote
  where bquote = char '`'

pbody :: Name -> Body -> Doc
pbody op (NormalB e) = text op <+> pprint e
pbody op (GuardedB xs) = vcat (map pguard xs)
  where pguard (g, e) = sep
          [ char '|'
          , pprint g
          , text op
          , pprint e ]

pclause :: Name -> Maybe Name -> Clause -> Doc
pclause op var (pats, body, decs) = eqn $$ tab where_
  where eqn = hsep
          [ pprint (fmap pid var)
          , sep (map pprint pats)
          , pbody op body ]
        where_ = if null decs
          then empty
          else text "where" <+> vcat (map pprint decs)

pmatch :: Match -> Doc
pmatch (pat, body, decs) = pclause "->" Nothing ([pat], body, decs)

instance Pretty Dec where
  pprint dec = case dec of
    FunD var xs -> vcat (map (pclause "=" (Just var)) xs)
    ValD pat body decs -> pclause "=" Nothing ([pat], body, decs)
    DataD tc tvs dcs -> hsep
      [ text "data"
      , sep (map text (tc:tvs))
      , sep (zipWith (<+>) ops (map pprint dcs)) ]
      where ops = equals:repeat (char '|')
    SigD vars ty -> hsep
      [ commaSep (map pid vars)
      , text "::"
      , pprint ty ]
    InfixD fixity ops -> pprint fixity <+> commaSep (map psym ops)

instance Pretty Exp where
  pprint e = case e of
    VarE var -> pid var
    ConE con -> pid con
    LitE lit -> pprint lit
    AppE e1 e2 -> sep
      [ pprintPrec (pr - 1) e1
      , pprintPrec pr e2 ]
    InfixE e1 op e2 -> sep
      [ pprintPrec pr e1
      , psym op
      , pprintPrec (pr - 1) e2 ]
    UInfixE e1 op e2 -> pprint (InfixE (Just e1) op (Just e2))
    ParensE e -> parens (pprint e)
    LamE pats e -> hsep
      [ char '\\' <> sep (map pprint pats) -- p_apat
      , text "->"
      , pprintPrec (pr - 1) e ]
    TupE exps -> parens (commaSep exps)
    ListE exps -> brackets (commaSep exps)
    CondE e0 e1 e2 -> sep
      [ text "if" <+> pprintPrec (pr - 1) e0
      , tab (text "then" <+> pprintPrec (pr - 1) e1)
      , tab (text "else" <+> pprintPrec (pr - 1) e2) ]
    CaseE e alts -> vcat
      [ text "case" <+> pprintPrec (pr - 1) e <+> text "of"
      , tab (vcat (map pmatch alts)) ]
    LetE decs e -> sep
      [ text "let" <+> vcat (map pprint decs)
      , text "in" <+> pprintPrec (pr - 1) e ]
    SigE e ty -> sep
      [ pprintPrec pr e
      , text "::"
      , pprint ty ]
    AsE var pat -> hcat
      [ pid var
      , char '@'
      , pprintPrec (pr - 1) pat ]
    WildE -> char '_'
    where pr = precOf e
  precOf e = case e of
    LSecE _ _ -> 0
    RSecE _ _ -> 0
    SigE _ _ -> 0
    LamE _ _ -> 1
    CondE _ _ _ -> 1
    CaseE _ _ -> 1
    LetE _ _ -> 1
    InfixE _ _ _ -> 1
    UInfixE _ _ _ -> 1
    AppE _ _ -> 2
    _ -> 3 -- atomic

instance Pretty Lit where
  pprint = text . show

instance Pretty Con where
  pprint con = case con of
    NormalC dc tys -> pprint (foldl TyAp (TyCon dc) tys)
    InfixC ty1 op ty2 -> hsep [pprint ty1, psym op, pprint ty2]

instance Pretty Fixity where
  pprint (Fixity assoc prec) = pprint assoc <+> int prec

instance Pretty Assoc where
  pprint = text . show
