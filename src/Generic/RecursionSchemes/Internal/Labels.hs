{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Generic.RecursionSchemes.Internal.Labels where

import GHC.OverloadedLabels
import GHC.TypeLits

import Generic.RecursionSchemes.Internal.Sum (Handler)
import Generic.RecursionSchemes.Internal.Generic

data Pattern (s :: Symbol) = Pattern

-- | We append @"_"@ because overloaded labels are currently parsed like
-- identifiers first, with a lowercase initial.
instance (s ~ AppendSymbol "_" s') => IsLabel s (Pattern s') where
  fromLabel = Pattern

-- | An infix variant of 'match_' to specify the constructor name via
-- the @OverloadedLabels@ extension.
--
-- @
-- 'case_'
--   (  \#_MyConstr0 '-->'          e
--   '|.' \#_MyConstr1 '-->' (\\a   -> f a)
--   '|.' \#_MyConstr2 '-->' (\\a b -> g a b)
--   )
-- @
(-->)
  :: forall c rs s s' a z f
  .  MatchSumCurried c rs s s' a z f
  => Pattern c -> f -> Handler a z s s'
(-->) _ = match_ @c @rs

infix 3 -->
