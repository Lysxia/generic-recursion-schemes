{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Foldable.Generic where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Function (fix)
import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits

import Data.Vinyl
import Data.Vinyl.TypeLevel

mapRec
  :: forall c rs f g
  .  AllConstrained c rs
  => (forall a. c a => f a -> g a) -> Rec f rs -> Rec g rs
mapRec _ RNil = RNil
mapRec f (r :& rs) = f r :& mapRec @c f rs

class IsBool b where
  bool' :: ((b ~ 'True) => r) -> ((b ~ 'False) => r) -> r

instance IsBool 'True where
  bool' a _ = a

instance IsBool 'False where
  bool' _ b = b

class (eqab ~ (a == b)) => DecEq' a b eqab where
  decEq :: ((a ~ b) => r) -> ((a == b) ~ 'False => r) -> r

instance (a ~ b) => DecEq' a (b :: *) 'True where
  decEq a _ = a

instance ('False ~ (a == b)) => DecEq' a b 'False where
  decEq _ b = b

type DecEq a b = DecEq' a b (a == b)

type family FromMaybe a (m :: Maybe *) where
  FromMaybe a 'Nothing  = a
  FromMaybe a ('Just b) = b

class IsMaybe m where
  maybe' :: ((m ~ 'Nothing) => r) -> (forall b. (m ~ 'Just b) => r) -> r

instance IsMaybe 'Nothing where
  maybe' a _ = a

instance IsMaybe ('Just b) where
  maybe' _ a = a @b

newtype FromMaybeF a m = FromMaybeF (FromMaybe a m)

mapFromMaybeF
  :: forall m a b
  .  IsMaybe m
  => (a -> b) -> FromMaybeF a m -> FromMaybeF b m
mapFromMaybeF f (FromMaybeF m) = FromMaybeF (maybe' @m (f m) m)

newtype BaseConF rs a = BaseConF { unBaseConF :: Rec (FromMaybeF a) rs }

instance AllConstrained IsMaybe rs => Functor (BaseConF rs) where
  fmap f (BaseConF r) = BaseConF (mapRec @IsMaybe (mapFromMaybeF f) r)

type ToRec e f = ToRec' e f '[]

type family ToRec' (e :: *) (f :: k -> *) (rs :: [Maybe *]) :: [Maybe *]
type instance ToRec' e (f :*: g) rs = ToRec' e f (ToRec' e g rs)
type instance ToRec' e (M1 i c (K1 j r)) rs =
  (If (e == r) 'Nothing ('Just r)) ': rs
type instance ToRec' e U1 rs = rs

type ToSum e f = ToSum' e f '[]

type family ToSum' e (f :: k -> *) (rs :: [(Symbol, * -> *)]) :: [(Symbol, * -> *)]
type instance ToSum' e (f :+: g) rs = ToSum' e f (ToSum' e g rs)
type instance ToSum' e (M1 D c f) rs = ToSum' e f rs
type instance ToSum' e (M1 C ('MetaCons cname x y) f) rs = '(cname, BaseConF (ToRec e f)) ': rs
type instance ToSum' e V1 rs = rs

data Sum (rs :: [(Symbol, * -> *)]) a where
  Here :: f a -> Sum ('(s, f) ': rs) a
  There :: Sum rs a -> Sum ('(s, f) ': rs) a

type family AllFunctorSnd (rs :: [(Symbol, * -> *)]) :: Constraint where
  AllFunctorSnd '[] = ()
  AllFunctorSnd ('(s, f) ': rs) = (Functor f, AllFunctorSnd rs)

instance AllFunctorSnd rs => Functor (Sum rs) where
  fmap :: (a -> b) -> Sum rs a -> Sum rs b
  fmap f (Here a) = Here (fmap f a)
  fmap f (There a) = There (fmap f a)

class RepToSum e f where
  repToSum :: f p -> Sum (ToSum e f) e

class RepToSum' e f rs where
  repToSum' :: Either (f p) (Sum rs e) -> Sum (ToSum' e f rs) e

class RepToProduct' e f rs where
  repToProduct'
    :: f p -> Rec (FromMaybeF e) rs -> Rec (FromMaybeF e) (ToRec' e f rs)

instance RepToSum' e f '[] => RepToSum e (M1 D i f) where
  repToSum (M1 f) = repToSum' @_ @_ @'[] (Left f)

instance (RepToSum' e f (ToSum' e g rs), RepToSum' e g rs)
  => RepToSum' e (f :+: g) rs where
  repToSum' (Left (L1 f)) = repToSum' @_ @_ @(ToSum' e g rs) (Left f)
  repToSum' (Left (R1 g)) = repToSum' @_ @f (Right (repToSum' @_ @_ @rs (Left g)))
  repToSum' (Right s) = repToSum' @_ @f (Right (repToSum' @_ @g (Right s)))

instance RepToProduct' e f '[] => RepToSum' e (M1 C ('MetaCons cname x y) f) rs where
  repToSum' (Left (M1 f)) = Here (BaseConF (repToProduct' f RNil))
  repToSum' (Right s) = There s

instance RepToSum' e V1 rs where
  repToSum' (Left v) = case v of {}
  repToSum' (Right s) = s

instance (RepToProduct' e f (ToRec' e g rs), RepToProduct' e g rs)
  => RepToProduct' e (f :*: g) rs where
  repToProduct' (f :*: g) rs = repToProduct' f (repToProduct' g rs)

instance DecEq e r => RepToProduct' e (M1 i c (K1 j r)) rs where
  repToProduct' (M1 (K1 r)) rs = decEq @e @r
    (FromMaybeF r :& rs)
    (FromMaybeF r :& rs)

instance RepToProduct' e U1 rs where
  repToProduct' U1 rs = rs

type GBaseF e = Sum (ToSum e (Rep e))

class RepToSum a (Rep a) => GToSum a
instance RepToSum a (Rep a) => GToSum a

project :: (Generic a, GToSum a) => a -> GBaseF a a
project = repToSum . from

cata :: (Generic a, GToSum a, Functor (GBaseF a)) => (GBaseF a r -> r) -> a -> r
cata f = fix $ \cata_f -> f . fmap cata_f . project

class Match c f rs rs' where
  match :: (f a -> z) -> (Sum rs' a -> z) -> Sum rs a -> z

instance Match0 c f c0 f0 rs rs' (c == c0)
  => Match c f ('(c0, f0) ': rs) rs' where
  match = match0 @c

class (eq ~ (c == c0)) => Match0 c f c0 f0 rs rs' eq where
  match0 :: (f a -> z) -> (Sum rs' a -> z) -> Sum ('(c0, f0) ': rs) a -> z

instance (c ~ c0, f ~ f0, rs ~ rs') => Match0 c f c0 f0 rs rs' 'True where
  match0 f _ (Here a) = f a
  match0 _ g (There a) = g a

instance ('False ~ (c == c0), rs' ~ ('(c0, f0) ': rs''), Match c f rs rs'')
  => Match0 c f c0 f0 rs rs' 'False where
  match0 _ g (Here a) = g (Here a)
  match0 f g (There a) = match @c f (g . There) a

type family MapFromMaybe a (rs :: [Maybe *]) :: [*] where
  MapFromMaybe a '[] = '[]
  MapFromMaybe a (r ': rs) = FromMaybe a r ': MapFromMaybe a rs

mapFromMaybe
  :: Rec (FromMaybeF a) rs -> Rec Identity (MapFromMaybe a rs)
mapFromMaybe RNil = RNil
mapFromMaybe (FromMaybeF r :& rs) = Identity r :& mapFromMaybe rs

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

match'
  :: forall c t z a rs ss ss'
  .  (Match c (BaseConF rs) ss ss', FromRec (MapFromMaybe a rs) t)
  => (t -> z) -> (Sum ss' a -> z) -> Sum ss a -> z
match' f = match @c @(BaseConF rs) (f . fromRec . mapFromMaybe . unBaseConF)

class UncurryRec rs z f where
  uncurryRec :: f -> (Rec Identity rs -> z)

instance (f ~ z) => UncurryRec '[] z f where
  uncurryRec z RNil = z

instance (f ~ (r -> f'), UncurryRec rs z f') => UncurryRec (r ': rs) z f where
  uncurryRec f (Identity r :& rs) = uncurryRec (f r) rs

match_
  :: forall c z f a rs ss ss'
  .  (Match c (BaseConF rs) ss ss', UncurryRec (MapFromMaybe a rs) z f)
  => f -> (Sum ss' a -> z) -> Sum ss a -> z
match_ f = match @c @(BaseConF rs) (uncurryRec f . mapFromMaybe . unBaseConF)

case_ :: Sum '[] a -> b
case_ v = case v of {}
