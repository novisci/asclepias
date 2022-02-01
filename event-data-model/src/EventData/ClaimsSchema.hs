{-|
Module      : Event Data Model Domains 
-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EventData.ClaimsSchema
  ( ClaimsSchema(..)
  , module EventData.Domain.Demographics
  , module EventData.Domain.Eligibility
  , module EventData.Domain.Enrollment
  , module EventData.Domain.Death
  , module EventData.Domain.Diagnosis
  , module EventData.Domain.Labs
  , module EventData.Domain.Medication
  ) where

import           Data.Aeson
import           Data.Eq                        ( Eq )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

import           EventData.Domain.Death
import           EventData.Domain.Demographics
import           EventData.Domain.Diagnosis
import           EventData.Domain.Eligibility
import           EventData.Domain.Enrollment
import           EventData.Domain.Labs
import           EventData.Domain.Medication
import           EventData.Domain.Procedure

-- | Defines the available domains.
data ClaimsSchema =
      Death DeathFacts
    | Demographics DemographicsFacts
    | Diagnosis DiagnosisFacts
    | Eligibility EligibilityFacts
    | Enrollment EnrollmentFacts
    | Labs LabsFacts
    | Medication MedicationFacts
    | Procedure ProcedureFacts
    deriving ( Eq, Show, Generic )

instance FromJSON ClaimsSchema where
  parseJSON = genericParseJSON
    (defaultOptions
      { sumEncoding = TaggedObject { tagFieldName      = "domain"
                                   , contentsFieldName = "facts"
                                   }
      }
    )
