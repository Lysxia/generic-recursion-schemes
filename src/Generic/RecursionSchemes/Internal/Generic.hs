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

-- | Catamorphisms with "GHC.Generics".

module Generic.RecursionSchemes.Internal.Generic where

import Data.Functor.Identity
import Data.Function (fix)
import Data.Type.Bool
import GHC.Generics
import GHC.TypeLits

import Data.Vinyl

import Generic.RecursionSchemes.Internal.Sum hiding (match)
import qualified Generic.RecursionSchemes.Internal.Sum as Sum
import Generic.RecursionSchemes.Internal.TyFun
import Generic.RecursionSchemes.Internal.Vinyl

-- | The base functor of a generic type @a@.
--
-- For example with lists, the base functor of @[a]@ is isomorphic to:
--
-- @
-- -- From recursion-schemes
-- data ListF a x = Nil | Cons a x
-- @
--
-- With generic-recursion-schemes, the equivalent construction
-- can be derived with 'GBase'.
--
-- @
-- type ListF a = 'GBase' ['Identity' a]
-- @
--
-- Note that this implementation, based on "GHC.Generics", has trouble with
-- parametric types, and it is often necessary to wrap type parameters
-- in 'Identity' and to apply coercions in a few places.
type GBase a = Sum (ToSum a (Rep a))

-- | Unwrap the base functor.
--
-- For example with lists, this is equivalent to:
--
-- @
-- -- With ListF from recursion-schemes
-- project :: [a] -> ListF a [a]
-- project []       = Nil
-- project (a : as) = Cons a as
-- @
gproject :: (Generic a, GToSum a) => a -> GBase a a
gproject = repToSum . from

-- | Fold a recursive structure.
--
-- For example, 'Data.List.foldr' is equivalent to:
--
-- @
-- -- With ListF and cata from recursion-schemes
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f b = cata $ \\case
--   Nil      -> b
--   Cons a b -> f a b
-- @
--
-- With generic-recursion-schemes, this can be written as
--
-- @
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f b = 'gcata' alg . ('Data.Coerce.coerce' :: [a] -> ['Identity' a]) where
--   alg = 'case_'
--     'Data.Function.&' 'match' \@\"[]\" (\\() -> b)
--     'Data.Function.&' 'match' \@\":\"  (\\(a, b) -> f a b)
-- @
gcata :: (Generic a, GToSum a, Functor (GBase a)) => (GBase a r -> r) -> a -> r
gcata f = fix $ \cata_f -> f . fmap cata_f . gproject

-- | One branch in a pattern-match construct for a base functor represented
-- as an extensible 'Sum'; the branch is given as an uncurried function.
--
-- See also 'match_'.
--
-- For a sum equivalent to this type:
--
-- @
-- data MyType
--   = MyConstr0
--   | MyConstr1 Int
--   | MyConstr2 MyType MyType
--   deriving 'Generic'
-- @
--
-- Pattern-matching looks like this:
--
-- @
-- 'case_'
--   'Data.Function.&' 'match' \@\"MyConstr0\" (\\()     -> e)      -- 0 field: () (or any equivalent 'Generic' type)
--   'Data.Function.&' 'match' \@\"MyConstr1\" (\\a      -> f a)    -- 1 field: unwrapped
--   'Data.Function.&' 'match' \@\"MyConstr2\" (\\(a, b) -> g a b)  -- 2 fields or more: tuple (or any equivalent 'Generic' product type)
--   -- in any order
--   :: 'GBase' MyType x -> y
-- @
match
  :: forall c t z a rs ss ss'
  .  (Match c (BaseConF rs) ss ss', FromRec (MapFromMaybe a rs) t)
  => (t -> z) -> (Sum ss' a -> z) -> Sum ss a -> z
match f = Sum.match @c @(BaseConF rs) (f . fromRec . mapRecFromMaybe . unBaseConF)

-- | One branch in a pattern-match construct for a base functor represented as an
-- extensible 'Sum'; the branch is given as a curried function.
--
-- See also 'match'.
--
-- @
-- 'case_'
--   'Data.Function.&' 'match_' \@\"MyConstr0\"          e
--   'Data.Function.&' 'match_' \@\"MyConstr1\" (\\a   -> f a)
--   'Data.Function.&' 'match_' \@\"MyConstr2\" (\\a b -> g a b)
--   -- in any order
--   :: 'GBase' MyType x -> y
-- @
match_
  :: forall c z f a rs ss ss'
  .  (Match c (BaseConF rs) ss ss', UncurryRec (MapFromMaybe a rs) z f)
  => f -> (Sum ss' a -> z) -> Sum ss a -> z
match_ f = Sum.match @c @(BaseConF rs) (uncurryRec f . mapRecFromMaybe . unBaseConF)

-- | Recursion schemes for generic types.
class RepToSum a (Rep a) => GToSum a
instance RepToSum a (Rep a) => GToSum a

type family MapFromMaybe a (rs :: [Maybe *]) :: [*] where
  MapFromMaybe a '[] = '[]
  MapFromMaybe a (r ': rs) = FromMaybe a r ': MapFromMaybe a rs

mapRecFromMaybe
  :: Rec (FromMaybeF a) rs -> Rec Identity (MapFromMaybe a rs)
mapRecFromMaybe RNil = RNil
mapRecFromMaybe (FromMaybeF r :& rs) = Identity r :& mapRecFromMaybe rs

newtype FromMaybeF a m = FromMaybeF (FromMaybe a m)

mapFromMaybeF
  :: forall m a b
  .  IsMaybe m
  => (a -> b) -> FromMaybeF a m -> FromMaybeF b m
mapFromMaybeF f (FromMaybeF m) = FromMaybeF (maybe' @m (f m) m)

foldFromMaybeF
  :: forall m a b
  .  IsMaybe m
  => (a -> b -> b) -> FromMaybeF a m -> b -> b
foldFromMaybeF f (FromMaybeF m) = maybe' @m (f m) id

traverseFromMaybeF
  :: forall m a b f
  .  (IsMaybe m, Applicative f)
  => (a -> f b) -> FromMaybeF a m -> f (FromMaybeF b m)
traverseFromMaybeF f (FromMaybeF m) = FromMaybeF <$> maybe' @m (f m) (pure m)

newtype BaseConF rs a = BaseConF { unBaseConF :: Rec (FromMaybeF a) rs }

instance MapRec IsMaybe rs => Functor (BaseConF rs) where
  fmap f (BaseConF r) = BaseConF (mapRec @IsMaybe (mapFromMaybeF f) r)

instance FoldRec IsMaybe rs => Foldable (BaseConF rs) where
  foldr f b (BaseConF r) = foldRec @IsMaybe (foldFromMaybeF f) b r

instance (MapRec IsMaybe rs, FoldRec IsMaybe rs, TraverseRec IsMaybe rs)
  => Traversable (BaseConF rs) where
  traverse f (BaseConF r) = BaseConF <$> traverseRec @IsMaybe (traverseFromMaybeF f) r

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
  {-# INLINE repToProduct' #-}

instance RepToProduct' e U1 rs where
  repToProduct' U1 rs = rs
