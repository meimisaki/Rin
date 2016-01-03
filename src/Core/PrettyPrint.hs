{-# LANGUAGE FlexibleInstances #-}

module Core.PrettyPrint
( Show (..)
) where

import Common

import Core.AST
import Text.PrettyPrint

instance {-# OVERLAPS #-} Show (Expr Name) where
  show = show . pprintExpr

instance {-# OVERLAPS #-} Show (Alter Name) where
  show = show . pprintAlter

instance {-# OVERLAPS #-} Show (Supercomb Name) where
  show = show . pprintSC

instance {-# OVERLAPS #-} Show (Program Name) where
  show = show . pprint

pprint :: Program Name -> Doc
pprint = vcat . map pprintSC . getProgram

tab :: Int
tab = 2

indent :: Doc -> Doc
indent doc = nest tab doc <> nest (-tab) empty

pprintSC :: Supercomb Name -> Doc
pprintSC (Supercomb name args body) = sep
  [ text name <+> hsep (map text args) <+> equals
  , indent (pprintExpr body) ]

parens' :: Bool -> Doc -> Doc
parens' False = parens
parens' True = id

pprintExpr :: Expr Name -> Doc
pprintExpr e = case e of
  EVar v -> text v
  ENum n -> int n
  EConstr tag arity -> text "Pack" <> braces (int tag <> comma <+> int arity)
  EAp e1 e2 -> sep [doc1, doc2]
    where doc1 = parens' (isAtomic e1 || isAp e1) (pprintExpr e1)
          doc2 = parens' (isAtomic e2) (pprintExpr e2)
  ELet rec defs body -> sep
    [ text (if rec then "letrec" else "let")
    , indent (vcat (map pprintDef defs))
    , text "in" <+> pprintExpr body ]
    where pprintDef (x, e) = sep [text x <+> equals, indent (pprintExpr e)]
  ECase e alts -> sep
    [ text "case" <+> pprintExpr e <+> text "of"
    , indent (vcat (map pprintAlter alts)) ]
  EAbs args body -> sep
    [ char '\\' <> hsep (map text args) <+> text "->"
    , indent (pprintExpr body) ]

pprintAlter :: Alter Name -> Doc
pprintAlter (Alter tag xs body) = sep
  [ char '<' <> int tag <> char '>' <+> hsep (map text xs) <+> text "->"
  , indent (pprintExpr body) ]
