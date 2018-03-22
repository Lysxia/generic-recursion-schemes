{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Extensible sum.

module Generic.RecursionSchemes.Internal.Sum where

import GHC.TypeLits

import Generic.RecursionSchemes.Internal.TyFun (type (==))

-- | A sum of functors.
data Sum (rs :: [(Symbol, * -> *)]) a where
  Here :: f a -> Sum ('(s, f) ': rs) a
  There :: Sum rs a -> Sum ('(s, f) ': rs) a

-- | Pattern match on the empty sum.
-- This can be used together with 'Generic.RecursionSchemes.match' and derived functions
-- to emulate a @case@ expression.
case_ :: Sum '[] a -> b
case_ v = case v of {}

instance FunctorSum rs => Functor (Sum rs) where
  fmap :: (a -> b) -> Sum rs a -> Sum rs b
  fmap = fmapSum

class FunctorSum rs where
  fmapSum :: (a -> b) -> Sum rs a -> Sum rs b

instance FunctorSum '[] where
  fmapSum _ = case_

instance (Functor f, FunctorSum rs) => FunctorSum ('(s, f) ': rs) where
  fmapSum f (Here a) = Here (fmap f a)
  fmapSum f (There a) = There (fmapSum f a)

instance FoldableSum rs => Foldable (Sum rs) where
  foldr = foldrSum

class FoldableSum rs where
  foldrSum :: (a -> b -> b) -> b -> Sum rs a -> b

instance FoldableSum '[] where
  foldrSum _ _ = case_

instance (Foldable f, FoldableSum rs) => FoldableSum ('(s, f) ': rs) where
  foldrSum f b (Here a) = foldr f b a
  foldrSum f b (There a) = foldrSum f b a

instance TraversableSum rs => Traversable (Sum rs) where
  traverse = traverseSum

class (FunctorSum rs, FoldableSum rs) => TraversableSum rs where
  traverseSum :: Applicative m => (a -> m b) -> Sum rs a -> m (Sum rs b)

instance TraversableSum '[] where
  traverseSum _ = case_

instance (Traversable f, TraversableSum rs) => TraversableSum ('(s, f) ': rs) where
  traverseSum f (Here a) = Here <$> traverse f a
  traverseSum f (There a) = There <$> traverseSum f a

class Match c f rs rs' where
  -- | Pattern-matching on a sum.
  --
  -- @
  -- 'case_'
  --   '&' 'match' \@\"C0\" (\('Data.Functor.Identity.Identity' a) -> [a])
  --   '&' 'match' \@\"C1\" ('id' :: [a] -> [a])
  --   '&' 'match' \@\"C2\" 'Data.Maybe.maybeToList'
  --   -- in any order
  --   :: 'Sum' '[ '(\"C0\", 'Data.Functor.Identity.Identity'), '(\"C1\", []), '(\"C2\", 'Maybe')] a -> [a]
  -- @
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
