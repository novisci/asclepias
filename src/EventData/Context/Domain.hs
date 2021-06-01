{-|
Module      : Event Data Model facts 
Description : Defines the Context type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}
module EventData.Context.Domain(
    Domain(..)
    , _Demographics
    , module EventData.Context.Domain.Demographics
) where

import Prelude                                  ( Show, Eq
                                                , ($), (<$>), (<*>), fail, drop
                                                , pure )
import Control.Lens                             ( makePrisms )
import GHC.Generics                             ( Generic )
import Data.Aeson
import Data.Foldable
import Data.Text                                ( Text, empty )
import EventData.Context.Domain.Demographics

data Domain =
      Demographics DemographicsFacts
    | UnimplementedDomain ()
    deriving ( Eq, Show, Generic )

makePrisms ''Domain

instance FromJSON Domain where
    parseJSON = withObject "Domain" $ \o -> do
        domain :: Text <- o .: "domain"
        case domain of
            "Demographics" -> Demographics <$> o .: "facts"
            _              -> pure (UnimplementedDomain ())


        