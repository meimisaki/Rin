module Type.PrettyPrint
( Pretty (..)
) where

import Common

import Data.Unique
import Text.PrettyPrint
import Type.Types

instance Show Type where
  show = show . pprint

instance Show TyVar where
  show = getTyVarName

instance Show (Meta a) where
  show (Meta uniq _) = show (hashUnique uniq)

instance Pretty TyVar where
  pprint = text . show

instance Pretty (Meta a) where
  pprint = text . show

precedence :: Type -> Int
precedence ty = case ty of
  TyForall _ _ -> 0
  TyArr _ _ -> 1
  TyAp _ _ -> 2
  _ -> 3 -- atomic

instance Pretty Type where
  pprint = go minBound
    where go pr ty
            | pr >= pr' = parens (go' pr' ty)
            | otherwise = go' pr' ty
            where pr' = precedence ty
          go' pr ty = case ty of
            TyForall [] rho -> pprint rho
            TyForall tvs rho -> sep
              [ text "forall" <+> hsep (map pprint tvs) <> char '.'
              , pprint rho ]
            TyArr sigma1 sigma2 -> sep
              [ go pr sigma1 <+> text "->"
              , go (pr - 1) sigma2 ]
            TyCon tc -> text tc
            TyVar tv -> pprint tv
            TyAp tau1 tau2 -> sep
              [ go (pr - 1) tau1
              , go pr tau2 ]
            TyMeta tv -> pprint tv
