{-# LANGUAGE PatternSynonyms #-}

module Type.Types
( Term (..)
, module Control.Monad.Except
, module Control.Monad.State
, TI
, runTI
, throwTI
, newTIRef
, readTIRef
, writeTIRef
, TIEnv (..)
, extendTyEnv
, lookupTyEnv
, extendKnEnv
, lookupKnEnv
, Type (..)
, mkForall
, Sigma (..)
, Rho (..)
, Tau (..)
, Kind
, pattern KnStar
, newTyVar
, TyVar (..)
, isBound
, getTyVarName
, newSkolemTyVar
, TyMeta
, Meta (..)
, newMeta
, readMeta
, writeMeta
) where

import Common

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import Data.IORef
import Data.Unique
import qualified Data.Map as M

data Term
  = TmVar Name
  | TmLit Int
  | TmAp Term Term
  | TmAbs Name Term
  | TmAnnotAbs Name Type Term
  | TmLet Name Term Term
  | TmAnnot Term Type

type TI a = ExceptT String (StateT TIEnv IO) a

runTI :: TI a -> TIEnv -> IO (Either String a, TIEnv)
runTI = runStateT . runExceptT

throwTI :: Show a => a -> TI b
throwTI = throwError . show

newTIRef :: a -> TI (IORef a)
newTIRef = liftIO . newIORef

readTIRef :: IORef a -> TI a
readTIRef = liftIO . readIORef

writeTIRef :: IORef a -> a -> TI ()
writeTIRef ref = liftIO . writeIORef ref

data TIEnv = TIEnv
  { tyEnv :: M.Map Name Type
  , knEnv :: M.Map Name Kind }

extendTyEnv :: Name -> Type -> TI a -> TI a
extendTyEnv name ty ti = do
  env <- get
  put env { tyEnv = M.insert name ty (tyEnv env) }
  ti <* put env

lookupTyEnv :: Name -> TI Type
lookupTyEnv name = do
  env <- gets tyEnv
  case M.lookup name env of
    Nothing -> throwError ("Variable not in scope: " ++ name)
    Just ty -> return ty

extendKnEnv :: Name -> Kind -> TI a -> TI a
extendKnEnv name kind ti = do
  env <- get
  put env { knEnv = M.insert name kind (knEnv env) }
  ti <* put env

lookupKnEnv :: Name -> TI Kind
lookupKnEnv name = do
  env <- gets knEnv
  case M.lookup name env of
    Nothing -> throwError ("Type variable not in scope: " ++ name)
    Just kind -> return kind

data Type
  = TyForall [TyVar] Rho
  | TyArr Type Type
  | TyCon Name
  | TyVar TyVar
  | TyAp Tau Tau -- predicativity
  | TyMeta TyMeta

mkForall :: [TyVar] -> Rho -> Sigma
mkForall [] = id
mkForall tvs = TyForall tvs

type Sigma = Type

type Rho = Type

type Tau = Type

type Kind = Type

pattern KnStar = TyCon "*"

newTyVar :: TI Tau
newTyVar = fmap TyMeta newMeta

data TyVar
  = Bound Name
  | Skolem Name Unique

instance Eq TyVar where
  Bound x1 == Bound x2 = x1 == x2
  Skolem _ u1 == Skolem _ u2 = u1 == u2
  _ == _ = False

instance Ord TyVar where
  Bound x1 `compare` Bound x2 = x1 `compare` x2
  Bound _ `compare` Skolem _ _ = LT
  Skolem _ u1 `compare` Skolem _ u2 = u1 `compare` u2
  Skolem _ _ `compare` Bound _ = GT

isBound :: TyVar -> Bool
isBound (Bound _) = True
isBound _ = False

getTyVarName :: TyVar -> Name
getTyVarName (Bound x) = x
getTyVarName (Skolem x _) = x

newSkolemTyVar :: TyVar -> TI TyVar
newSkolemTyVar tv = do
  uniq <- liftIO newUnique
  return (Skolem (getTyVarName tv) uniq)

type TyMeta = Meta Tau -- predicativity

data Meta a = Meta Unique (IORef (Maybe a))

instance Eq (Meta a) where
  Meta u1 _ == Meta u2 _ = u1 == u2

instance Ord (Meta a) where
  Meta u1 _ `compare` Meta u2 _ = u1 `compare` u2

newMeta :: TI (Meta a)
newMeta = do
  uniq <- liftIO newUnique
  ref <- newTIRef Nothing
  return (Meta uniq ref)

readMeta :: (Meta a) -> TI (Maybe a)
readMeta (Meta _ ref) = readTIRef ref

writeMeta :: (Meta a) -> a -> TI ()
writeMeta (Meta _ ref) = writeTIRef ref . Just
