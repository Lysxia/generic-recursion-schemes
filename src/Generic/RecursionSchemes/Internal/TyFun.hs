-- | Type-level functions.

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

module Generic.RecursionSchemes.Internal.TyFun where

import Data.Type.Equality

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

