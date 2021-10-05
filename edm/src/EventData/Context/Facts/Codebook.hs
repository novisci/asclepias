{-|
Module      : Codebook fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Facts.Codebook(
  Codebook(..)
) where
import Data.Aeson               ( FromJSON )
import Data.Eq                  ( Eq )
import Data.Maybe               ( Maybe )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )

data Codebook = 
    CDT
    | CPT
    | HCPCS
    | ICD9
    | ICD9PC
    | ICD10
    | ICD10PC
    | LOINC
    | MedicaidCat 
    -- TODO: MedicaidCat currently won't be parsed b/c data is  "medicaid_cat" ==> Codebook needs custom FromJSON instance
    | NABSP
    | NDC
    | NDC9 
    | UB92 
    | USSTATE
    -- TODO: USSTATE currently won't be parsed b/c data is  "US_STATE" ==> Codebook needs custom FromJSON instance
   deriving (Eq, Show, Generic)

instance FromJSON Codebook where