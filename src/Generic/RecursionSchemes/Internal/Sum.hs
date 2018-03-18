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

import Data.Kind
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

-- | Constraint that all elements in the list are functors.
--
-- Used to define the 'Functor' instance for 'Sum'.
type family AllSnd (c :: (* -> *) -> Constraint) (rs :: [(Symbol, * -> *)])
  :: Constraint where
  AllSnd c '[] = ()
  AllSnd c ('(s, f) ': rs) = (c f, AllSnd c rs)

instance AllSnd Functor rs => Functor (Sum rs) where
  fmap :: (a -> b) -> Sum rs a -> Sum rs b
  fmap f (Here a) = Here (fmap f a)
  fmap f (There a) = There (fmap f a)

instance (AllSnd Functor rs, AllSnd Foldable rs) => Foldable (Sum rs) where
  foldr f b (Here a) = foldr f b a
  foldr f b (There a) = foldr f b a

instance (AllSnd Functor rs, AllSnd Foldable rs, AllSnd Traversable rs)
  => Traversable (Sum rs) where
  traverse f (Here a) = Here <$> traverse f a
  traverse f (There a) = There <$> traverse f a

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
