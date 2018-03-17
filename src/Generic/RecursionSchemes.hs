-- | Catamorphisms with "GHC.Generics".

module Generic.RecursionSchemes
  ( -- * Generic catamorphisms
    cata

  , GBaseF
  , project
  , GToSum

    -- * Base functor representation

  , Sum(..)
  , case_
  , match
  , match_

    -- *** Reexported from base
  , (&)

    -- ** Inner wrappers
  , BaseConF(..)
  , FromMaybeF(..)
  ) where

import Data.Function ((&))
import Generic.RecursionSchemes.Internal.Generic
import Generic.RecursionSchemes.Internal.Sum hiding (match)
