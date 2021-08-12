{-|
Module      : Hasklepias
Description : Everything you should need to get up and running with 
              hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

See the examples folder and manual for further documentation.
-}

module Hasklepias (
      module EventData
    
    -- * Working with Features
    , module Features

    -- * Utilities for defining Features from Events
    {- |
    Much of logic needed to define features from events depends on the 
    [interval-algebra](https://hackage.haskell.org/package/interval-algebra) library.
    Its main functions and types are re-exported in Hasklepias, but the documentation
    can be found on [hackage](https://hackage.haskell.org/package/interval-algebra).

    -}
    , module FeatureEvents
    
    -- * Specifying and building cohorts
    , module Hasklepias.Cohort

    -- ** Creating an executable cohort application
    , module Hasklepias.MakeApp

    -- * Statistical Types
    , module Stype

    -- * Rexported Functions and modules
    , module Hasklepias.Reexports
    , module Hasklepias.ReexportsUnsafe
) where

import EventData

import IntervalAlgebra
import IntervalAlgebra.IntervalUtilities
import IntervalAlgebra.PairedInterval

import Features

import FeatureEvents
import Hasklepias.Reexports
import Hasklepias.ReexportsUnsafe
import Hasklepias.Cohort
import Hasklepias.MakeApp

import Stype