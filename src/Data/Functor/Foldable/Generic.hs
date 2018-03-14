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

newtype BaseConF rs a = BaseConF (Rec (FromMaybeF a) rs)

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
