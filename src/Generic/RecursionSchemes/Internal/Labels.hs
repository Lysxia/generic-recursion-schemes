{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | A prettier syntax for pattern-matching extensible sums using overloaded
-- labels.

module Generic.RecursionSchemes.Internal.Labels where

import GHC.OverloadedLabels
import GHC.TypeLits

data Pattern (s :: Symbol) = Pattern

instance (s ~ AppendSymbol "_" s') => IsLabel s (Pattern s') where
  fromLabel = Pattern
