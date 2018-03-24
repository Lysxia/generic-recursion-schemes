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
  , caseDefault
  , caseDefaultOf
  , (|.)
  , match
  , match_

    -- ** Constructors
  , con
  , con_

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

import Generic.RecursionSchemes.Internal.Generic
import Generic.RecursionSchemes.Internal.Sum (Sum(..), Handler(..), (|.))
import Generic.RecursionSchemes.Internal.Vinyl (Lazy(..), LazyT)
