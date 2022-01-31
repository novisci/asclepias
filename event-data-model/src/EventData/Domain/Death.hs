{-|
Module      : Event Data Death Domain 
-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Domain.Death
  ( DeathFacts(..)
  ) where

import           Data.Aeson                     ( FromJSON(..)
                                                , withObject
                                                )
import           Data.Eq                        ( Eq )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

-- | A death fact
data DeathFacts = DeathFacts
  deriving (Eq, Show, Generic)

{- 
By default, the generic FromJSON assumes a void data constructor
to be from a JSON array.
The withOjbect here forces it be an empty JSON object.
-}
instance FromJSON DeathFacts where
  parseJSON = withObject "Death" $ \o -> do
    pure DeathFacts
