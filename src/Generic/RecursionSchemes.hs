-- | Catamorphisms with "GHC.Generics".

module Generic.RecursionSchemes
  ( -- * Generic catamorphisms
    gcata
  , gproject

    -- * Generic anamorphisms
  , gana
  , gembed

    -- * Base functor representation

  , GBase()

    -- ** Destructors
  , Handler()
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
  ) where

import Generic.RecursionSchemes.Internal.Generic
import Generic.RecursionSchemes.Internal.Sum (Handler(), (|.))
