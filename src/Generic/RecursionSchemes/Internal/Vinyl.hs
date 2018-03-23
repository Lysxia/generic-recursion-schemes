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


class ToRec rs t where
  toRec :: t -> Rec Identity rs

instance (r ~ r') => ToRec '[r] r' where
  toRec r = Identity r :& RNil

instance (Generic t, GToRec '[] t) => ToRec '[] t where
  toRec = gToRec

instance (Generic t, GToRec (r1 ': r2 ': rs) t) => ToRec (r1 ': r2 ': rs) t where
  toRec = gToRec

type GToRec rs a = GToRec' rs '[] (Rep a)

gToRec :: (Generic a, GToRec rs a) => a -> Rec Identity rs
gToRec a = gToRec' (from a) RNil

class GToRec' rs rs' f where
  gToRec' :: f p -> Rec Identity rs' -> Rec Identity rs

instance GToRec' rs rs' f => GToRec' rs rs' (M1 i c f) where
  gToRec' = gToRec' . unM1

instance (GToRec' rs rs' f, GToRec' rs' rs'' g) => GToRec' rs rs'' (f :*: g) where
  gToRec' (f :*: g) = gToRec' f . gToRec' @rs' g

instance (rs ~ (r ': rs')) => GToRec' rs rs' (K1 i r) where
  gToRec' (K1 r) rs = Identity r :& rs

instance (rs ~ rs') => GToRec' rs rs' U1 where
  gToRec' _ = id


class UncurryRec rs z f where
  uncurryRec :: f -> (Rec Identity rs -> z)

instance (f ~ z) => UncurryRec '[] z f where
  uncurryRec z RNil = z

instance (f ~ (r -> f'), UncurryRec rs z f') => UncurryRec (r ': rs) z f where
  uncurryRec f (Identity r :& rs) = uncurryRec (f r) rs


class CurryRec rs z f where
  curryRec :: (Rec Identity rs -> z) -> f

type family IsFunction f :: Bool where
  IsFunction (a -> b) = 'True
  IsFunction c = 'False

instance CurryRec' rs z f (IsFunction f) => CurryRec rs z f where
  curryRec = curryRec'

class (IsFunction f ~ isfun) => CurryRec' rs z f isfun where
  curryRec' :: (Rec Identity rs -> z) -> f

instance (rs ~ '[], f ~ z, IsFunction f ~ 'False) => CurryRec' rs z f 'False where
  curryRec' z = z RNil

instance (rs ~ (r ': rs'), CurryRec rs' z f) => CurryRec' rs z (r -> f) 'True where
  curryRec' f r = curryRec (\rs -> f (Identity r :& rs))
