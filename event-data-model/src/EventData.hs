{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
-}
{-# LANGUAGE NoImplicitPrelude #-}
module EventData
  ( -- ** Event Data Schema
    module EventData.ClaimsSchema
    -- ** Predicates
  , module EventData.ClaimsSchema.Predicates
    -- ** Accessing data in Events
  , module EventData.ClaimsSchema.Accessors
  ) where

import           EventData.ClaimsSchema
import           EventData.ClaimsSchema.Accessors
import           EventData.ClaimsSchema.Predicates

