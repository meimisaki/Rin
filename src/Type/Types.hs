module Type.Types
( Term (..)
, module Control.Monad.Except
, module Control.Monad.State
, TI
, runTI
, newTIRef
, readTIRef
, writeTIRef
, TIEnv
, extendEnv
, lookupVar
, Sigma (..)
, Rho (..)
, Tau (..)
, sigmaToRho
, rhoToSigma
, tauToSigma
, newTyVar
, TyVar (..)
, isBound
, getTyVarName
, newSkolemTyVar
, TyMeta (..)
, newMetaTyVar
, readTyMeta
, writeTyMeta
, TyRef
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

newTIRef :: a -> TI (IORef a)
newTIRef = liftIO . newIORef

readTIRef :: IORef a -> TI a
readTIRef = liftIO . readIORef

writeTIRef :: IORef a -> a -> TI ()
writeTIRef ref = liftIO . writeIORef ref

type TIEnv = M.Map Name Sigma

extendEnv :: Name -> Sigma -> TI a -> TI a
extendEnv name sigma ti = do
  env <- get
  put (M.insert name sigma env)
  ti <* put env

lookupVar :: Name -> TI Sigma
lookupVar name = do
  env <- get
  case M.lookup name env of
    Nothing -> throwError ("Not in scope: " ++ name)
    Just sigma -> return sigma

data Sigma = TyForall [TyVar] Rho

data Rho
  = TyMono Tau
  | TyArr Sigma Sigma

-- TODO: higher-kinded types
data Tau
  = TyCon Name
  | TyVar TyVar
  | TyArr0 Tau Tau
  | TyMeta TyMeta

sigmaToRho :: Sigma -> Rho
sigmaToRho (TyForall [] rho) = rho

rhoToSigma :: Rho -> Sigma
rhoToSigma = TyForall []

tauToSigma :: Tau -> Sigma
tauToSigma = rhoToSigma . TyMono

newTyVar :: TI Tau
newTyVar = fmap TyMeta newMetaTyVar

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

data TyMeta = Meta Unique TyRef

instance Eq TyMeta where
  Meta u1 _ == Meta u2 _ = u1 == u2

instance Ord TyMeta where
  Meta u1 _ `compare` Meta u2 _ = u1 `compare` u2

newMetaTyVar :: TI TyMeta
newMetaTyVar = do
  uniq <- liftIO newUnique
  ref <- newTIRef Nothing
  return (Meta uniq ref)

readTyMeta :: TyMeta -> TI (Maybe Tau)
readTyMeta (Meta _ ref) = readTIRef ref

writeTyMeta :: TyMeta -> Tau -> TI ()
writeTyMeta (Meta _ ref) = writeTIRef ref . Just

type TyRef = IORef (Maybe Tau)
