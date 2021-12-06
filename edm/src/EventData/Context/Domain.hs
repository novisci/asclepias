{-|
Module      : Event Data Model Domains 
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Context.Domain
  ( Domain(..)
  , module EventData.Context.Domain.Demographics
  , module EventData.Context.Domain.Eligibility
  , module EventData.Context.Domain.Enrollment
  , module EventData.Context.Domain.Death
  , module EventData.Context.Domain.Diagnosis
  , module EventData.Context.Domain.Labs
  , module EventData.Context.Domain.Medication
  ) where

import           Data.Eq                        ( Eq )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

import           EventData.Context.Domain.Death
import           EventData.Context.Domain.Demographics
import           EventData.Context.Domain.Diagnosis
import           EventData.Context.Domain.Eligibility
import           EventData.Context.Domain.Enrollment
import           EventData.Context.Domain.Labs
import           EventData.Context.Domain.Medication
import           EventData.Context.Domain.Procedure

-- | Defines the available domains.
data Domain =
      Death DeathFacts
    | Demographics DemographicsFacts
    | Diagnosis DiagnosisFacts
    | Eligibility EligibilityFacts
    | Enrollment EnrollmentFacts
    | Labs LabsFacts
    | Medication MedicationFacts
    | Procedure ProcedureFacts
    | UnimplementedDomain ()
    deriving ( Eq, Show, Generic )

