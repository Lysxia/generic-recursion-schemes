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

import Control.Category (Category)
import qualified Control.Category as Category
import GHC.TypeLits

import Generic.RecursionSchemes.Internal.TyFun (type (==))

-- | A tagged sum of functors.
data Sum (rs :: [(Symbol, * -> *)]) a where
  Here :: f a -> Sum ('(s, f) ': rs) a
  There :: Sum rs a -> Sum ('(s, f) ': rs) a

-- | Pattern match on the empty sum.
emptySum :: Sum '[] a -> b
emptySum v = case v of {}

-- This can be used together with 'Generic.RecursionSchemes.match'
-- or 'Generic.RecursionSchemes.match_' to emulate a @case@ expression.

instance FunctorSum rs => Functor (Sum rs) where
  fmap :: (a -> b) -> Sum rs a -> Sum rs b
  fmap = fmapSum

class FunctorSum rs where
  fmapSum :: (a -> b) -> Sum rs a -> Sum rs b

instance FunctorSum '[] where
  fmapSum _ = emptySum

instance (Functor f, FunctorSum rs) => FunctorSum ('(s, f) ': rs) where
  fmapSum f (Here a) = Here (fmap f a)
  fmapSum f (There a) = There (fmapSum f a)

instance FoldableSum rs => Foldable (Sum rs) where
  foldr = foldrSum

class FoldableSum rs where
  foldrSum :: (a -> b -> b) -> b -> Sum rs a -> b

instance FoldableSum '[] where
  foldrSum _ _ = emptySum

instance (Foldable f, FoldableSum rs) => FoldableSum ('(s, f) ': rs) where
  foldrSum f b (Here a) = foldr f b a
  foldrSum f b (There a) = foldrSum f b a

instance TraversableSum rs => Traversable (Sum rs) where
  traverse = traverseSum

class (FunctorSum rs, FoldableSum rs) => TraversableSum rs where
  traverseSum :: Applicative m => (a -> m b) -> Sum rs a -> m (Sum rs b)

instance TraversableSum '[] where
  traverseSum _ = emptySum

instance (Traversable f, TraversableSum rs) => TraversableSum ('(s, f) ': rs) where
  traverseSum f (Here a) = Here <$> traverse f a
  traverseSum f (There a) = There <$> traverseSum f a


-- | Handler for a sum type.
--
-- @s@ is the list of alternatives to handled, @s'@ is the list of alternatives
-- that are /not/ handled by this handler (and so will be passed to another
-- handler), @a@ is the sum functor's parameter, and @z@ is the result type of
-- the handler.
--
-- A total handler (handling all cases) has the last argument equal to @'[]@:
-- @'Handler' a z s '[]@.
newtype Handler a z s s' = Handler
  { unHandler :: (Sum s' a -> z) -> Sum s a -> z }

-- | The categorical composition is @'flip' ('|.')@.
instance Category (Handler a z) where
  id = Handler id
  (.) = flip (|.)

-- | Apply a total handler.
case_ :: Handler a z s '[] -> Sum s a -> z
case_ (Handler h) = h emptySum

-- | Flipped 'case_' so the scrutinee may appear first.
caseOf :: Sum s a -> Handler a z s '[] -> z
caseOf = flip case_

-- | Composition of handlers: the first handler passes what it can't handle to
-- the next one.
(|.) :: Handler a z s s' -> Handler a z s' s'' -> Handler a z s s''
Handler h1 |. Handler h2 = Handler (h1 . h2)

-- | Handle one alternative.
match :: forall c f a s s' z. Match c f s s' => (f a -> z) -> Handler a z s s'
match = Handler . match_ @c

-- | Default handler that always matches.
default_ :: (Sum s a -> z) -> Handler a z s '[]
default_ h = Handler (const h)


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
  match_ :: (f a -> z) -> (Sum rs' a -> z) -> Sum rs a -> z

instance Match0 c f c0 f0 rs rs' (c == c0)
  => Match c f ('(c0, f0) ': rs) rs' where
  match_ = match0 @c

class (eq ~ (c == c0)) => Match0 c f c0 f0 rs rs' eq where
  match0 :: (f a -> z) -> (Sum rs' a -> z) -> Sum ('(c0, f0) ': rs) a -> z

instance (c ~ c0, f ~ f0, rs ~ rs') => Match0 c f c0 f0 rs rs' 'True where
  match0 f _ (Here a) = f a
  match0 _ g (There a) = g a

instance ('False ~ (c == c0), rs' ~ ('(c0, f0) ': rs''), Match c f rs rs'')
  => Match0 c f c0 f0 rs rs' 'False where
  match0 _ g (Here a) = g (Here a)
  match0 f g (There a) = match_ @c f (g . There) a

class Construct c f rs where
  con :: f a -> Sum rs a

instance Construct0 c f c0 f0 rs (c == c0)
  => Construct c f ('(c0, f0) ': rs) where
  con = con0 @c

class (eq ~ (c == c0)) => Construct0 c f c0 f0 rs eq where
  con0 :: f a -> Sum ('(c0, f0) ': rs) a

instance (c ~ c0, f ~ f0) => Construct0 c f c0 f0 rs 'True where
  con0 = Here

instance ('False ~ (c == c0), Construct c f rs)
  => Construct0 c f c0 f0 rs 'False where
  con0 = There . con @c
