{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
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

module Generic.RecursionSchemes.Internal.Generic where

import Data.Functor.Identity
import Data.Function (fix)
import Data.Type.Bool
import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits

import Data.Vinyl
import Data.Vinyl.TypeLevel

import Generic.RecursionSchemes.Internal.Sum hiding (match)
import qualified Generic.RecursionSchemes.Internal.Sum as Sum
import Generic.RecursionSchemes.Internal.TyFun

mapRec
  :: forall c rs f g
  .  AllConstrained c rs
  => (forall a. c a => f a -> g a) -> Rec f rs -> Rec g rs
mapRec _ RNil = RNil
mapRec f (r :& rs) = f r :& mapRec @c f rs

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

type family MapFromMaybe a (rs :: [Maybe *]) :: [*] where
  MapFromMaybe a '[] = '[]
  MapFromMaybe a (r ': rs) = FromMaybe a r ': MapFromMaybe a rs

mapFromMaybe
  :: Rec (FromMaybeF a) rs -> Rec Identity (MapFromMaybe a rs)
mapFromMaybe RNil = RNil
mapFromMaybe (FromMaybeF r :& rs) = Identity r :& mapFromMaybe rs

match
  :: forall c t z a rs ss ss'
  .  (Match c (BaseConF rs) ss ss', FromRec (MapFromMaybe a rs) t)
  => (t -> z) -> (Sum ss' a -> z) -> Sum ss a -> z
match f = Sum.match @c @(BaseConF rs) (f . fromRec . mapFromMaybe . unBaseConF)

match_
  :: forall c z f a rs ss ss'
  .  (Match c (BaseConF rs) ss ss', UncurryRec (MapFromMaybe a rs) z f)
  => f -> (Sum ss' a -> z) -> Sum ss a -> z
match_ f = Sum.match @c @(BaseConF rs) (uncurryRec f . mapFromMaybe . unBaseConF)
