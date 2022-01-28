{-|
Module      : Event Data Procedure Domain
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Context.Domain.Procedure
  ( ProcedureFacts(..)
  ) where

import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           EventData.Context.Facts
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

data ProcedureFacts = ProcedureFacts
  { code            :: Code
  , claim           :: Maybe Claim
  , location        :: Maybe Location
  , provider        :: Maybe Provider
  , hospitalization :: Maybe Hospitalization
  }
  deriving (Eq, Show, Generic)


instance FromJSON ProcedureFacts where
