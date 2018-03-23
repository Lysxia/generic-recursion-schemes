-- | Catamorphisms with "GHC.Generics".

module Generic.RecursionSchemes
  ( -- * Generic catamorphisms
    gcata
  , gproject
  , GToSum

    -- * Generic anamorphisms
  , gana
  , gembed
  , GFromSum

    -- * Base functor representation

  , GBase

  , Sum(..)

    -- ** Destructors
  , case_
  , match
  , match_

    -- ** Constructors
  , con
  , con_

    -- *** Reexported from base
  , (&)

    -- ** Inner wrappers
  , BaseConF(..)
  , FromMaybeF(..)
  ) where

import Data.Function ((&))
import Generic.RecursionSchemes.Internal.Generic
import Generic.RecursionSchemes.Internal.Sum hiding (match, con)
