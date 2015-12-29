module Data.NameSupply
( NameSupply (..)
, mkNameSupply
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

getFreshName :: NameSupply -> (Name, NameSupply)
getFreshName (NameSupply (name:xs)) = (name, NameSupply xs)
