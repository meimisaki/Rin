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
  pprint (Supercomb name args body) = hsep
    [ text name
    , sep (map text args)
    , equals
    , tab (pprint body) ]

instance Pretty (Expr Name) where
  pprint e = case e of
    EVar v -> text v
    ENum n -> int n
    EConstr tag arity -> text "Pack" <> braces (int tag <> comma <> int arity)
    EAp e1 e2 -> sep
      [ pprintPrec (pr - 1) e1
      , pprintPrec pr e2 ]
    ELet rec defs body -> sep
      [ text (if rec then "letrec" else "let") <+> vcat (map pprintDef defs)
      , text "in" <+> pprint body ]
      where pprintDef (x, e) = text x <+> equals <+> pprint e
    ECase e alts -> sep
      [ text "case" <+> pprint e <+> text "of"
      , tab (vcat (map pprint alts)) ]
    EAbs args body -> hsep
      [ char '\\' <> sep (map text args)
      , text "->"
      , tab (pprint body) ]
    where pr = precOf e
  precOf e = case e of
    EAbs _ _ -> 0
    ELet _ _ _ -> 0
    ECase _ _ -> 0
    EAp _ _ -> 1
    _ -> 2 -- atomic

instance Pretty (Alter Name) where
  pprint (Alter tag xs body) = hsep
    [ char '<' <> int tag <> char '>'
    , sep (map text xs)
    , text "->"
    , tab (pprint body) ]
