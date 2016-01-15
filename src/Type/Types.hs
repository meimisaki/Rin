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
, Kind (..)
, newKnVar
, Sigma (..)
, Rho (..)
, Tau (..)
, pattern TyArr0
, sigmaToRho
, rhoToSigma
, tauToSigma
, newTyVar
, TyVar (..)
, isBound
, getTyVarName
, newSkolemTyVar
, TyMeta
, KnMeta
, MetaVar (..)
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
  | TmAnnotAbs Name Sigma Term
  | TmLet Name Term Term
  | TmAnnot Term Sigma

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
  { tyEnv :: M.Map Name Sigma
  , knEnv :: M.Map Name Kind }

extendTyEnv :: Name -> Sigma -> TI a -> TI a
extendTyEnv name sigma ti = do
  env <- get
  put env { tyEnv = M.insert name sigma (tyEnv env) }
  ti <* put env

lookupTyEnv :: Name -> TI Sigma
lookupTyEnv name = do
  env <- gets tyEnv
  case M.lookup name env of
    Nothing -> throwError ("Variable not in scope: " ++ name)
    Just sigma -> return sigma

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

-- maybe we should merge `Kind`, `Sigma`, `Rho`, and `Tau` :)
data Kind
  = KnStar
  | KnArr Kind Kind
  | KnMeta KnMeta

newKnVar :: TI Kind
newKnVar = fmap KnMeta newMeta

data Sigma = TyForall [TyVar] Rho

data Rho
  = TyMono Tau
  | TyArr Sigma Sigma

data Tau
  = TyCon Name
  | TyVar TyVar
  | TyAp Tau Tau -- predicativity
  | TyMeta TyMeta

pattern TyArr0 a b = TyAp (TyAp (TyCon "->") a) b

sigmaToRho :: Sigma -> Rho
sigmaToRho (TyForall [] rho) = rho

rhoToSigma :: Rho -> Sigma
rhoToSigma = TyForall []

tauToSigma :: Tau -> Sigma
tauToSigma = rhoToSigma . TyMono

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

type TyMeta = MetaVar Tau -- predicativity

type KnMeta = MetaVar Kind

data MetaVar a = MetaVar Unique (IORef (Maybe a))

instance Eq (MetaVar a) where
  MetaVar u1 _ == MetaVar u2 _ = u1 == u2

instance Ord (MetaVar a) where
  MetaVar u1 _ `compare` MetaVar u2 _ = u1 `compare` u2

newMeta :: TI (MetaVar a)
newMeta = do
  uniq <- liftIO newUnique
  ref <- newTIRef Nothing
  return (MetaVar uniq ref)

readMeta :: (MetaVar a) -> TI (Maybe a)
readMeta (MetaVar _ ref) = readTIRef ref

writeMeta :: (MetaVar a) -> a -> TI ()
writeMeta (MetaVar _ ref) = writeTIRef ref . Just
