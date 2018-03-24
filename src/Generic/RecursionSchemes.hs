-- | Catamorphisms with "GHC.Generics".

module Generic.RecursionSchemes
  ( -- * Generic catamorphisms
    cataG
  , projectG

    -- * Generic anamorphisms
  , anaG
  , anaGM
  , anaGM0
  , embedG

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
