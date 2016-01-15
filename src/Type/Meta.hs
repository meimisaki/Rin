{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Type.Meta
( Meta (..)
, getMetaVars
) where

import qualified Data.Set as S
import Type.Types

class Ord (MetaOf a) => Meta a where
  type MetaOf a :: *
  zonk :: a -> TI a
  metaVars :: a -> S.Set (MetaOf a)

getMetaVars :: Meta a => a -> TI (S.Set (MetaOf a))
getMetaVars = fmap metaVars . zonk

instance Meta a => Meta [a] where
  type MetaOf [a] = MetaOf a
  zonk = mapM zonk
  metaVars = S.unions . map metaVars

instance Meta Kind where
  type MetaOf Kind = KnMeta
  zonk KnStar = return KnStar
  zonk (KnArr kind1 kind2) = do
    kind1' <- zonk kind1
    kind2' <- zonk kind2
    return (KnArr kind1' kind2')
  zonk kind@(KnMeta kv) = do
    mbKind <- readMeta kv
    case mbKind of
      Nothing -> return kind
      Just kind1 -> do
        kind2 <- zonk kind1
        writeMeta kv kind2
        return kind2
  metaVars kind = go kind S.empty
    where go KnStar = id
          go (KnArr kind1 kind2) = go kind1 . go kind2
          go (KnMeta kv) = S.insert kv

instance Meta Sigma where
  type MetaOf Sigma = TyMeta
  zonk (TyForall tvs rho) = do
    rho' <- zonk rho
    return (TyForall tvs rho')
  metaVars (TyForall _ rho) = metaVars rho

instance Meta Rho where
  type MetaOf Rho = TyMeta
  zonk (TyMono tau) = fmap TyMono (zonk tau)
  zonk (TyArr sigma1 sigma2) = do
    sigma1' <- zonk sigma1
    sigma2' <- zonk sigma2
    return (TyArr sigma1' sigma2')
  metaVars (TyMono tau) = metaVars tau
  metaVars (TyArr sigma1 sigma2) = S.union tvs1 tvs2
    where tvs1 = metaVars sigma1
          tvs2 = metaVars sigma2

instance Meta Tau where
  type MetaOf Tau = TyMeta
  zonk tau@(TyCon _) = return tau
  zonk tau@(TyVar _) = return tau
  zonk (TyAp tau1 tau2) = do
    tau1' <- zonk tau1
    tau2' <- zonk tau2
    return (TyAp tau1' tau2')
  zonk tau@(TyMeta tv) = do
    mbTau1 <- readMeta tv
    case mbTau1 of
      Nothing -> return tau
      Just tau1 -> do
        tau2 <- zonk tau1
        writeMeta tv tau2
        return tau2
  metaVars tau = go tau S.empty
    where go (TyCon _) = id
          go (TyVar _) = id
          go (TyAp tau1 tau2) = go tau1 . go tau2
          go (TyMeta tv) = S.insert tv
