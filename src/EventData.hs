{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EventData
  ( -- * Events
    module EventData.Core
    -- ** Event Contexts
  , module EventData.Context
    -- ** Event Domains
  , module EventData.Context.Domain
    -- ** Predicates
  , module EventData.Predicates
    -- ** Accessing data in Events
  , module EventData.Accessors
    -- ** Parsing Events
  , module EventData.Aeson
    -- ** Generating arbitrary events
  , module EventData.Arbitrary
  ) where

import           EventData.Aeson
import           EventData.Arbitrary
import           EventData.Context
import           EventData.Context.Domain
import           EventData.Core
import           EventData.Predicates
import           EventData.Accessors

