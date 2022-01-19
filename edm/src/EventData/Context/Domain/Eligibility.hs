{-|
Module      : Event Data Eligibility Domain 
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Context.Domain.Eligibility
  ( EligibilityFacts(..)
  ) where

import           Data.Aeson                     ( FromJSON(..) )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           EventData.Context.Facts.Plan   ( Plan )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

-- | Eligibility
newtype EligibilityFacts = EligibilityFacts {
     plan :: Maybe Plan
  }
  deriving( Eq, Show, Generic )

instance FromJSON EligibilityFacts where
