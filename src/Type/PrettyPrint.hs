module Type.PrettyPrint
( Pretty (..)
) where

import Common

import Data.Unique
import Text.PrettyPrint
import Type.Types

instance Show Sigma where
  show = show . pprint

instance Show Rho where
  show = show . pprint

instance Show Tau where
  show = show . pprint

instance Show TyVar where
  show = getTyVarName

instance Show TyMeta where
  show (Meta uniq _) = show (hashUnique uniq)

instance Pretty TyVar where
  pprint = text . show

instance Pretty TyMeta where
  pprint = text . show

instance Pretty Sigma where
  pprint (TyForall [] rho) = pprint rho
  pprint (TyForall tvs rho) = hsep
    [ text "forall"
    , hsep (map (text . show) tvs) <> char '.'
    , pprint rho ]

instance Pretty Rho where
  pprint (TyMono tau) = pprint tau
  pprint (TyArr sigma1 sigma2) = hsep
    [ parens' (pprint sigma1)
    , text "->"
    , pprint sigma2 ]
    where (TyForall _ rho) = sigma1
          parens' = case rho of
            TyMono (TyArr0 _ _) -> parens
            TyArr _ _ -> parens
            _ -> id

instance Pretty Tau where
  pprint (TyCon tc) = text tc
  pprint (TyVar tv) = text (show tv)
  pprint (TyArr0 tau1 tau2) = hsep
    [ parens' (pprint tau1)
    , text "->"
    , pprint tau2 ]
    where parens' = case tau1 of
            TyArr0 _ _ -> parens
            _ -> id
  pprint (TyMeta tv) = text (show tv)
