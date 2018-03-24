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

import Control.Monad ((>=>))
import Data.Bifunctor
import Data.Functor.Compose
import Data.Type.Bool
import GHC.Generics
import GHC.TypeLits

import Data.Vinyl

import Generic.RecursionSchemes.Internal.Sum hiding (match, con)
import qualified Generic.RecursionSchemes.Internal.Sum as Sum
import Generic.RecursionSchemes.Internal.TyFun
import Generic.RecursionSchemes.Internal.Vinyl hiding (ToRec, toRec)
import qualified Generic.RecursionSchemes.Internal.Vinyl as Vinyl

-- | The base functor of a generic type @a@.
--
-- Note that this implementation, based on "GHC.Generics", has trouble with
-- parametric types, and it is often necessary to wrap type parameters
-- in 'Identity' and to apply coercions in a few places.
--
-- === __Example__
--
-- With lists, the base functor of @[a]@ is isomorphic to:
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
type GBase a = Sum (ToSum a (Rep a))

-- | Unwrap the base functor.
--
-- === __Example__
--
-- With lists, 'gproject' is equivalent to:
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
-- === __Example__
--
-- 'Data.List.foldr' is equivalent to:
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
gcata f = gcata_f where gcata_f = f . fmap gcata_f . gproject

-- | Approximate, simplified signature:
--
-- @
-- match
--   :: forall cname t z ss1 ss2
--   .  _omittedConstraints
--   => (t -> z)
--   -> (Sum (ss1               ++ ss2) _ -> z)
--   -> (Sum (ss1 ++ (cname, _) ++ ss2) _ -> z)
-- @
--
-- 'match' must be applied to a constructor name as a type-level string (@cname@).
-- The first value-level argument (of type @t -> z@) is one branch in a
-- pattern-match construct for a base functor represented as an extensible
-- 'Sum'; the branch must be given as an uncurried function that takes a tuple
-- (in any type with a single constructor having the right number and types of
-- fields).
-- The second argument (@Sum (ss1 ++ ss2) _ -> z@) represents the remaining
-- branches.
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
--   'Data.Function.&' 'match' \@\"MyConstr2\" (\\(a, b) -> g a b)  -- 2 fields or more: tuple (or any equivalent 'Generic' product type)
--   'Data.Function.&' 'match' \@\"MyConstr1\" (\\a      -> f a)    -- 1 field: unwrapped
--   'Data.Function.&' 'match' \@\"MyConstr0\" (\\()     -> e)      -- 0 field: () (or any equivalent 'Generic' type)
--   -- in any order
--   :: 'GBase' MyType x -> y
-- @
match
  :: forall c t z a rs ss ss'
  .  ( Match c (BaseConF rs) ss ss'
     , FromRec (MapFromMaybe a rs) t
     , DistributeFromMaybe rs
     )
  => (t -> z) -> (Sum ss' a -> z) -> Sum ss a -> z
match f = Sum.match @c @(BaseConF rs) (f . fromRec . distributeFromMaybe . unBaseConF)

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
  .  ( Match c (BaseConF rs) ss ss'
     , UncurryRec (MapFromMaybe a rs) z f
     , DistributeFromMaybe rs )
  => f -> (Sum ss' a -> z) -> Sum ss a -> z
match_ f = Sum.match @c @(BaseConF rs) (uncurryRec f . distributeFromMaybe . unBaseConF)

-- | Recursion schemes for generic types.
class RepToSum a (Rep a) => GToSum a
instance RepToSum a (Rep a) => GToSum a


-- | Wrap the base functor.
--
-- === __Example__
--
-- With lists, 'gembed' is equivalent to:
--
-- @
-- -- With ListF from recursion-schemes
-- embed :: ListF a [a] -> [a]
-- embed Nil = []
-- embed (Cons a as) = a : as
-- @
gembed :: (Generic a, GFromSum a) => GBase a a -> a
gembed = to . sumToRep

-- | Unfold a corecursive structure.
--
-- === __Example__
--
-- Consider 'replicate':
--
-- @
-- replicate :: Int -> a -> [Int]
-- replicate 0 _ = []
-- replicate n a = replicate (n-1) a
-- @
--
-- With generic-recursion-schemes, this can be written as
--
-- @
-- replicate :: Int -> a -> [Int]
-- replicate n a = 'gana' alg n where
--   alg 0 = 'con_' \@\"[]\"
--   alg n = 'con_' \@\":\" a (n-1)
-- @
gana :: (Generic a, GFromSum a, Functor (GBase a)) => (r -> GBase a r) -> r -> a
gana f = gana_f where gana_f = gembed . fmap gana_f . f

-- | Construct a value in a base functor given a tuple (the constructor of any
-- single-constructor type with the right number and types of fields).
--
-- @
-- 'con' \@\":\" (3, Just 4) :: GBase [Int] (Maybe Int)
-- -- equivalent to (Cons 3 (Just 4) :: ListF Int (Maybe Int))
-- @
--
-- See also 'con_'.
con
  :: forall c t a rs ss
  .  ( Construct c (BaseConF rs) ss
     , Vinyl.ToRec (MapFromMaybe a rs) t
     , FactorFromMaybe rs )
  => t -> Sum ss a
con = Sum.con @c @(BaseConF rs) . BaseConF . factorFromMaybe . Vinyl.toRec

-- | Curried constructor of a base functor.
--
-- @
-- 'con_' \@\":\" 3 (Just 4) :: GBase [Int] (Maybe Int)
-- -- equivalent to (Cons 3 (Just 4) :: ListF Int (Maybe Int))
-- @
--
-- See also 'con'.
con_
  :: forall c a rs ss f
  .  ( Construct c (BaseConF rs) ss
     , CurryRec (MapFromMaybe a rs) (Sum ss a) f
     , FactorFromMaybe rs )
  => f
con_ = curryRec @(MapFromMaybe a rs) @(Sum ss a)
  (Sum.con @c @(BaseConF rs) @ss . BaseConF . factorFromMaybe)

-- | Corecursion schemes for generic types.
class SumToRep a (Rep a) => GFromSum a
instance SumToRep a (Rep a) => GFromSum a


type family MapFromMaybe a (rs :: [Maybe *]) :: [*] where
  MapFromMaybe a '[] = '[]
  MapFromMaybe a (r ': rs) = FromMaybe a r ': MapFromMaybe a rs

newtype FromMaybeF a m = FromMaybeF { unFromMaybeF :: FromMaybe a m }

class DistributeFromMaybe rs where
  distributeFromMaybe
    :: Rec (LazyT (FromMaybeF a)) rs -> Rec Lazy (MapFromMaybe a rs)

instance DistributeFromMaybe '[] where
  distributeFromMaybe RNil = RNil

instance DistributeFromMaybe rs => DistributeFromMaybe (r ': rs) where
  distributeFromMaybe (Compose r :& rs) =
    fmap unFromMaybeF r :& distributeFromMaybe rs

class FactorFromMaybe rs where
  factorFromMaybe :: Rec Lazy (MapFromMaybe a rs) -> Rec (LazyT (FromMaybeF a)) rs

instance FactorFromMaybe '[] where
  factorFromMaybe RNil = RNil

instance FactorFromMaybe rs => FactorFromMaybe (r ': rs) where
  factorFromMaybe (r :& rs) = Compose (fmap FromMaybeF r) :& factorFromMaybe rs

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

newtype BaseConF rs a =
  BaseConF { unBaseConF :: Rec (LazyT (FromMaybeF a)) rs }

instance MapRec IsMaybe rs => Functor (BaseConF rs) where
  fmap f (BaseConF r) = BaseConF (mapRec @IsMaybe (fmap1 (mapFromMaybeF f)) r)

instance FoldRec IsMaybe rs => Foldable (BaseConF rs) where
  foldr f b (BaseConF r) =
    foldRec @IsMaybe (foldFromMaybeF f . unLazy . getCompose) b r

instance (MapRec IsMaybe rs, FoldRec IsMaybe rs, TraverseRec IsMaybe rs)
  => Traversable (BaseConF rs) where
  traverse f (BaseConF r) =
    BaseConF <$> traverseRec @IsMaybe (traverse1 (traverseFromMaybeF f)) r


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
type instance ToSum' e (M1 C c f) rs = '(CName c, BaseConF (ToRec e f)) ': rs
type instance ToSum' e V1 rs = rs

type family CName (c :: Meta) :: Symbol
type instance CName ('MetaCons cname x y) = cname

class RepToSum e f where
  repToSum :: f p -> Sum (ToSum e f) e

class RepToSum' e f rs where
  repToSum' :: Either (f p) (Sum rs e) -> Sum (ToSum' e f rs) e

class RepToProduct' e f rs where
  repToProduct'
    :: f p -> Rec (LazyT (FromMaybeF e)) rs -> Rec (LazyT (FromMaybeF e)) (ToRec' e f rs)

instance RepToSum' e f '[] => RepToSum e (M1 D i f) where
  repToSum (M1 f) = repToSum' @_ @_ @'[] (Left f)

instance (RepToSum' e f (ToSum' e g rs), RepToSum' e g rs)
  => RepToSum' e (f :+: g) rs where
  repToSum' (Left (L1 f)) = repToSum' @_ @_ @(ToSum' e g rs) (Left f)
  repToSum' (Left (R1 g)) = repToSum' @_ @f (Right (repToSum' @_ @_ @rs (Left g)))
  repToSum' (Right s) = repToSum' @_ @f (Right (repToSum' @_ @g (Right s)))

instance RepToProduct' e f '[] => RepToSum' e (M1 C c f) rs where
  repToSum' (Left (M1 f)) = Here (BaseConF (repToProduct' f RNil))
  repToSum' (Right s) = There s

instance RepToSum' e V1 rs where
  repToSum' (Left v) = case v of {}
  repToSum' (Right s) = s

instance (RepToProduct' e f (ToRec' e g rs), RepToProduct' e g rs)
  => RepToProduct' e (f :*: g) rs where
  repToProduct' (f :*: g) rs = repToProduct' f (repToProduct' g rs)

instance DecEq e r => RepToProduct' e (M1 i c (K1 j r)) rs where
  repToProduct' (M1 (K1 r)) rs =
    Compose (Lazy (FromMaybeF (decEq @e @r r r))) :& rs

instance RepToProduct' e U1 rs where
  repToProduct' U1 rs = rs

class SumToRep e f where
  sumToRep :: Sum (ToSum e f) e -> f p

class SumToRep' e f rs where
  sumToRep' :: Sum (ToSum' e f rs) e -> Either (f p) (Sum rs e)

class ProductToRep' e f rs where
  productToRep'
    :: Rec (LazyT (FromMaybeF e)) (ToRec' e f rs)
    -> (f p, Rec (LazyT (FromMaybeF e)) rs)

instance SumToRep' e f '[] => SumToRep e (M1 D c f) where
  sumToRep s = case sumToRep' @_ @_ @'[] s of
    Left f -> M1 f
    Right s' -> case s' of {}

instance (SumToRep' e f (ToSum' e g rs), SumToRep' e g rs)
  => SumToRep' e (f :+: g) rs where
  sumToRep' =
    (first L1 . (sumToRep' @_ @_ @(ToSum' e g rs)))
      >=> (first R1 . sumToRep' @_ @_ @rs)

instance ProductToRep' e f '[] => SumToRep' e (M1 C c f) rs where
  sumToRep' (Here (BaseConF s)) =
    let (f, RNil) = productToRep' @_ @_ @'[] s in
    Left (M1 f)
  sumToRep' (There s) = Right s

instance SumToRep' e V1 rs where
  sumToRep' = Right

instance (ProductToRep' e f (ToRec' e g rs), ProductToRep' e g rs)
  => ProductToRep' e (f :*: g) rs where
  productToRep' s =
    let (f, s') = productToRep' @_ @_ @(ToRec' e g rs) s
        (g, s'') = productToRep' @_ @_ @rs s' in
    (f :*: g, s'')

instance DecEq e r => ProductToRep' e (M1 i c (K1 j r)) rs where
  productToRep' (Compose (Lazy (FromMaybeF r)) :& rs) = (M1 (K1 (decEq @e @r r r)), rs)

instance ProductToRep' e U1 rs where
  productToRep' rs = (U1, rs)
