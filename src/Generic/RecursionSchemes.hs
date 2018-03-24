-- | Catamorphisms with "GHC.Generics".

module Generic.RecursionSchemes
  ( -- * Generic catamorphisms
    gcata
  , gproject

    -- * Generic anamorphisms
  , gana
  , ganaM
  , ganaM0
  , gembed

    -- * Base functor representation

  , GBase()
  , GBaseSum

    -- ** Destructors
  , Handler()
  , (|.)
  , case_
  , caseDefault
  , caseOf
  , caseDefaultOf
  , match
  , match_

    -- ** Constructors
  , con
  , con_
  ) where

import Generic.RecursionSchemes.Internal.Generic
import Generic.RecursionSchemes.Internal.Sum (Handler(), (|.))
