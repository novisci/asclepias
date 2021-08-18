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

module EventData.Context.Domain.Enrollment(
  EnrollmentFacts(..)
) where

import Control.Lens             ( makeLenses )
import Data.Aeson               ( FromJSON(..)
                                , genericParseJSON
                                , defaultOptions
                                , fieldLabelModifier )
import Data.List                ( drop )
import Data.Eq                  ( Eq )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )

-- data Plan = Plan 

-- | An enrollment fact
newtype EnrollmentFacts = EnrollmentFacts {
     _plan :: () -- TODO add plan fact
  } 
  deriving( Eq, Show, Generic )

makeLenses ''EnrollmentFacts

instance FromJSON EnrollmentFacts where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
