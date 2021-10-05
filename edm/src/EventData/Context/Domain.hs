{-|
Module      : Event Data Model Domains 
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Domain(
    Domain(..)
    , module EventData.Context.Domain.Demographics
    , module EventData.Context.Domain.Enrollment
    , module EventData.Context.Domain.Death
    , module EventData.Context.Domain.Diagnosis
) where

import Data.Eq                                  ( Eq )
import Data.Text                                ( Text, empty )
import GHC.Generics                             ( Generic )
import GHC.Show                                 ( Show )

import EventData.Context.Domain.Demographics
import EventData.Context.Domain.Enrollment
import EventData.Context.Domain.Death
import EventData.Context.Domain.Diagnosis

-- | Defines the available domains.
data Domain =
      Death DeathFacts
    | Demographics DemographicsFacts
    | Diagnosis DiagnosisFacts
    | Enrollment EnrollmentFacts
    | UnimplementedDomain ()
    deriving ( Eq, Show, Generic )

