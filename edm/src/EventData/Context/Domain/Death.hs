{-|
Module      : Event Data Death Domain 
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Domain.Death(
  DeathFacts(..)
) where

import Data.Aeson               ( FromJSON )
import Data.Eq                  ( Eq )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )

-- | An death fact
data DeathFacts = DeathFacts
  deriving( Eq, Show, Generic )

instance FromJSON DeathFacts where
