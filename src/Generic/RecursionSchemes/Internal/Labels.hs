{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | A prettier syntax for pattern-matching extensible sums using overloaded
-- labels.
--
-- Only for /base/ ≥ 4.10.

module Generic.RecursionSchemes.Internal.Labels where

import GHC.OverloadedLabels
import GHC.TypeLits

import Generic.RecursionSchemes.Internal.Sum (Handler)
import Generic.RecursionSchemes.Internal.Generic

data Pattern (s :: Symbol) = Pattern

instance (s ~ AppendSymbol "_" s') => IsLabel s (Pattern s') where
  fromLabel = Pattern

-- | An infix variant of 'match_' to specify the constructor name via
-- the @OverloadedLabels@ extension.
--
-- @
-- 'case_'
--   (  \#MyConstr0 '-->'          e
--   '|.' \#MyConstr1 '-->' (\\a   -> f a)
--   '|.' \#MyConstr2 '-->' (\\a b -> g a b)
--   )
-- @
(-->)
  :: forall c rs s s' a z f
  .  MatchSumCurried c rs s s' a z f
  => Pattern c -> f -> Handler a z s s'
(-->) _ = match_ @c @rs

infix 3 -->
