{-|
Module      : Event Data Enrollment Domain 
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Domain.Enrollment(
  EnrollmentFacts(..)
) where

import Data.Aeson               ( FromJSON(..) )
import Data.Eq                  ( Eq )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )

-- | An enrollment fact
newtype EnrollmentFacts = EnrollmentFacts {
     plan :: () -- TODO add plan fact
  } 
  deriving( Eq, Show, Generic )

instance FromJSON EnrollmentFacts where
