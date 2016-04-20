module Data.NameSupply
( NameSupply (..), mkNameSupply, getFreshName
, NS, runNS
, newName, findName, withName
) where

import Common

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

newtype NameSupply = NameSupply { getNames :: [Name] }

mkNameSupply :: S.Set Name -> NameSupply
mkNameSupply usedNames = NameSupply freeNames
  where mkName suffix = map (:suffix) ['a'..'z']
        allNames = concatMap mkName ("":map show [1..])
        freeNames = filter (\name -> S.notMember name usedNames) allNames

getFreshName :: NameSupply -> (Name, NameSupply)
getFreshName (NameSupply (name:xs)) = (name, NameSupply xs)

type NS = ReaderT (M.Map Name Name) (State NameSupply)

runNS :: NS a -> NameSupply -> (a, NameSupply)
runNS ns supply = runState (runReaderT ns M.empty) supply

newName :: NS Name
newName = do
  (name, supply) <- gets getFreshName
  put supply
  return name

findName :: Name -> NS Name
findName name = reader (M.findWithDefault name name)

withName :: Name -> Name -> NS a -> NS a
withName name name1 = local (M.insert name name1)
