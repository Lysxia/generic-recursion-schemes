{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.RecursionSchemes.Internal.Vinyl where

import Control.Applicative
import Data.Bifunctor
import Data.Functor.Identity
import GHC.Generics

import Data.Vinyl

class MapRec c rs where
  mapRec :: (forall a. c a => f a -> g a) -> Rec f rs -> Rec g rs

instance MapRec c '[] where
  mapRec _ RNil = RNil

instance (c a, MapRec c rs) => MapRec c (a ': rs) where
  mapRec f (r :& rs) = f r :& mapRec @c f rs

class FoldRec c rs where
  foldRec :: (forall a. c a => f a -> b -> b) -> b -> Rec f rs -> b

instance FoldRec c '[] where
  foldRec _ b RNil = b

instance (c a, FoldRec c rs) => FoldRec c (a ': rs) where
  foldRec f b (r :& rs) = f r (foldRec @c f b rs)

class TraverseRec c rs where
  traverseRec
    :: Applicative m
    => (forall a. c a => f a -> m (g a)) -> Rec f rs -> m (Rec g rs)

instance TraverseRec c '[] where
  traverseRec _ RNil = pure RNil

instance (c a, TraverseRec c rs) => TraverseRec c (a ': rs) where
  traverseRec f (r :& rs) = liftA2 (:&) (f r) (traverseRec @c f rs)

class FromRec rs t where
  fromRec :: Rec Identity rs -> t

instance (Generic t, GFromRec '[] t) => FromRec '[] t where
  fromRec = gFromRec

instance (r ~ r') => FromRec '[r] r' where
  fromRec (Identity a :& RNil) = a

instance (Generic t, GFromRec (r ': r2 ': rs) t) => FromRec (r ': r2 ': rs) t where
  fromRec = gFromRec

type GFromRec rs a = GFromRec' rs '[] (Rep a)

gFromRec :: (Generic a, GFromRec rs a) => Rec Identity rs -> a
gFromRec = to . fst . gFromRec' @_ @'[]

class GFromRec' rs rs' f where
  gFromRec' :: Rec Identity rs -> (f p, Rec Identity rs')

instance GFromRec' rs rs' f => GFromRec' rs rs' (M1 i c f) where
  gFromRec' = first M1 . gFromRec'

instance (GFromRec' rs rs' f, GFromRec' rs' rs'' g) => GFromRec' rs rs'' (f :*: g) where
  gFromRec' rs =
    let (f, rs') = gFromRec' rs
        (g, rs'') = gFromRec' @rs' rs'
    in (f :*: g, rs'')

instance (rs ~ (r ': rs')) => GFromRec' rs rs' (K1 i r) where
  gFromRec' (Identity r :& rs) = (K1 r, rs)

instance (rs ~ rs') => GFromRec' rs rs' U1 where
  gFromRec' rs = (U1, rs)

class UncurryRec rs z f where
  uncurryRec :: f -> (Rec Identity rs -> z)

instance (f ~ z) => UncurryRec '[] z f where
  uncurryRec z RNil = z

instance (f ~ (r -> f'), UncurryRec rs z f') => UncurryRec (r ': rs) z f where
  uncurryRec f (Identity r :& rs) = uncurryRec (f r) rs
