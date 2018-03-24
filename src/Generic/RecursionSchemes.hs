-- | Catamorphisms with "GHC.Generics".

module Generic.RecursionSchemes
  ( -- * Generic catamorphisms
    gcata
  , gproject

    -- * Generic anamorphisms
  , gana
  , gembed

    -- * Base functor representation

  , GBase

  , Sum(..)

    -- ** Destructors
  , Handler(..)
  , case_
  , caseOf
  , (|.)
  , match
  , match_
  , default_

    -- ** Constructors
  , con
  , con_

    -- *** Reexported from base
  , (&)

    -- * Infrastructure

    -- | Possibly useful interface to define pattern synonyms and some other
    -- generic combinators.

  , GToSum
  , GFromSum

    -- ** Inner wrappers
  , BaseConF(..)
  , FromMaybeF(..)
  , Lazy(..)
  , LazyT
  ) where

import Data.Function ((&))
import Generic.RecursionSchemes.Internal.Generic
import Generic.RecursionSchemes.Internal.Sum (Sum(..), Handler(..), (|.), default_)
import Generic.RecursionSchemes.Internal.Vinyl (Lazy(..), LazyT)
