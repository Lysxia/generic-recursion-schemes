-- | Extensible sum.

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

module Generic.RecursionSchemes.Internal.Sum where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Kind
import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits

import Data.Vinyl

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

case_ :: Sum '[] a -> b
case_ v = case v of {}
