{-|
Module      : Event Data Procedure Domain
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Context.Domain.Procedure(
  ProcedureFacts(..)
) where

import Data.Aeson               ( FromJSON )
import Data.Eq                  ( Eq )
import Data.Maybe               ( Maybe )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )
import EventData.Context.Facts

data ProcedureFacts = ProcedureFacts {
       code :: Code
     , claim :: Maybe Claim
     , location :: Maybe Location
  } 
  deriving( Eq, Show, Generic )


instance FromJSON ProcedureFacts where
