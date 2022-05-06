{-|
Module      : Event Data Diagnosis Domain
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Domain.Medication
  ( MedicationFacts(..)
  ) where

import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           EventData.Facts
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

-- | An enrollment fact
data MedicationFacts = MedicationFacts
  { code     :: Code
  , claim    :: Maybe Claim
  , location :: Maybe Location
  , fill     :: Maybe Fill
  , provider :: Maybe Provider
  }
  deriving (Eq, Show, Generic)


instance FromJSON MedicationFacts where
