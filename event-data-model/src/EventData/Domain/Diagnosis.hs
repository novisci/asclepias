{-|
Module      : Event Data Diagnosis Domain
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Domain.Diagnosis
  ( DiagnosisFacts(..)
  ) where

import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           EventData.Facts
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

-- | A Diagnosis domain
data DiagnosisFacts = DiagnosisFacts
  { code            :: Code
  , claim           :: Maybe Claim
  , location        :: Maybe Location
  , provider        :: Maybe Provider
  , hospitalization :: Maybe Hospitalization
  }
  deriving (Eq, Show, Generic)


instance FromJSON DiagnosisFacts where
