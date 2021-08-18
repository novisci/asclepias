{-|
Module      : Event Data Model facts 
Description : Defines the Context type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
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
    , _Enrollment
    , module EventData.Context.Domain.Enrollment
) where

import Control.Lens                             ( makePrisms )
import Data.Eq                                  ( Eq )
import Data.Text                                ( Text, empty )
import GHC.Generics                             ( Generic )
import GHC.Show                                 ( Show )

import EventData.Context.Domain.Demographics
import EventData.Context.Domain.Enrollment

-- | Defines the available domains.
data Domain =
      Demographics DemographicsFacts
    | Enrollment EnrollmentFacts
    | UnimplementedDomain ()
    deriving ( Eq, Show, Generic )

makePrisms ''Domain
