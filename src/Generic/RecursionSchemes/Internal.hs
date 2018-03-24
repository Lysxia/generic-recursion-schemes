-- |
-- Possibly useful interface to define pattern synonyms and some other
-- generic combinators.

module Generic.RecursionSchemes.Internal
  ( -- * Core types

    GBase(..)
  , GBaseSum
  , Sum(..)
  , Handler(..)

    -- * Core classes

  , GToSum
  , GFromSum

    -- * Inner wrappers

  , BaseConF(..)
  , FromMaybeF(..)
  , Lazy(..)
  , LazyT
  ) where

import Generic.RecursionSchemes.Internal.Sum (Sum(..), Handler(..))
import Generic.RecursionSchemes.Internal.Generic
import Generic.RecursionSchemes.Internal.Vinyl (Lazy(..), LazyT)
