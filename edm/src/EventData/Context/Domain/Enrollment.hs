{-|
Module      : Event Data Enrollment Domain 
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Context.Domain.Enrollment(
    EnrollmentFacts(..)
  , emptyEnrollmentFact
) where

import Data.Aeson               ( FromJSON(..) )
import Data.Eq                  ( Eq )
import Data.Maybe               ( Maybe(..) )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )
import EventData.Context.Facts.Plan ( Plan, emptyPlan )

-- | Enrollment
newtype EnrollmentFacts = EnrollmentFacts {
     plan :: Maybe Plan 
  } 
  deriving( Eq, Show, Generic )

emptyEnrollmentFact :: EnrollmentFacts
emptyEnrollmentFact = EnrollmentFacts Nothing

instance FromJSON EnrollmentFacts where
