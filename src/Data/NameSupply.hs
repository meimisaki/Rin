module Data.NameSupply
( NameSupply (..)
, mkNameSupply
, initialNameSupply
, getFreshName
) where

import Common

import qualified Data.Set as S

newtype NameSupply = NameSupply { getNames :: [Name] }

mkNameSupply :: S.Set Name -> NameSupply
mkNameSupply usedNames = NameSupply freeNames
  where mkName suffix = map (:suffix) ['a'..'z']
        allNames = concatMap mkName ("":map show [1..])
        freeNames = filter (\name -> S.notMember name usedNames) allNames

initialNameSupply :: NameSupply
initialNameSupply = mkNameSupply S.empty

getFreshName :: NameSupply -> (Name, NameSupply)
getFreshName (NameSupply (name:xs)) = (name, NameSupply xs)
