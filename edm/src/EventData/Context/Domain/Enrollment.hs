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
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )
import EventData.Context.Facts.Plan ( Plan, emptyPlan )

-- | Enrollment
newtype EnrollmentFacts = EnrollmentFacts {
     plan :: Plan 
  } 
  deriving( Eq, Show, Generic )

emptyEnrollmentFact :: EnrollmentFacts
emptyEnrollmentFact = EnrollmentFacts emptyPlan

instance FromJSON EnrollmentFacts where
