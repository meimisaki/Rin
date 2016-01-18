{-# LANGUAGE FlexibleInstances #-}

module Core.PrettyPrint
( Pretty (..)
) where

import Common

import Core.AST
import Text.PrettyPrint

instance {-# OVERLAPS #-} Show (Expr Name) where
  show = show . pprint

instance {-# OVERLAPS #-} Show (Alter Name) where
  show = show . pprint

instance {-# OVERLAPS #-} Show (Supercomb Name) where
  show = show . pprint

instance {-# OVERLAPS #-} Show (Program Name) where
  show = show . pprint

instance Pretty (Program Name) where
  pprint = vcat . map pprint . getProgram

instance Pretty (Supercomb Name) where
  pprint (Supercomb name args body) = sep
    [ text name <+> hsep (map text args) <+> equals
    , tab (pprint body) ]

instance Pretty (Expr Name) where
  pprint e = case e of
    EVar v -> text v
    ENum n -> int n
    EConstr tag arity -> text "Pack" <> braces (int tag <> comma <+> int arity)
    EAp e1 e2 -> sep [doc1, doc2]
      where doc1 = parens' (isAtomic e1 || isAp e1) (pprint e1)
            doc2 = parens' (isAtomic e2) (pprint e2)
            parens' False = parens
            parens' True = id
    ELet rec defs body -> sep
      [ text (if rec then "letrec" else "let")
      , tab (vcat (map pprintDef defs))
      , text "in" <+> pprint body ]
      where pprintDef (x, e) = sep [text x <+> equals, tab (pprint e)]
    ECase e alts -> sep
      [ text "case" <+> pprint e <+> text "of"
      , tab (vcat (map pprint alts)) ]
    EAbs args body -> sep
      [ char '\\' <> hsep (map text args) <+> text "->"
      , tab (pprint body) ]

instance Pretty (Alter Name) where
  pprint (Alter tag xs body) = sep
    [ char '<' <> int tag <> char '>' <+> hsep (map text xs) <+> text "->"
    , tab (pprint body) ]
