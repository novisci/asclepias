{-|
Module      : Hasklepias
Description : Everything you should need to get up and running with 
              hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

See the examples folder and manual for further documentation.
-}

module Hasklepias
  ( module EventData

    -- * Working with Features
  , module Features

    -- * Feature definition builders 
    {- | 
    A collection of pre-defined functions which build common feature definitions
    used in epidemiologic cohorts.
    -}

  , module Hasklepias.Templates
    -- * Utilities for defining Features from Events
    {- |
    Much of logic needed to define features from events depends on the 
    [interval-algebra](https://hackage.haskell.org/package/interval-algebra) library.
    Its main functions and types are re-exported in Hasklepias, but the documentation
    can be found on [hackage](https://hackage.haskell.org/package/interval-algebra).

    -}
  , module Hasklepias.FeatureEvents
  , module Hasklepias.Misc

    -- * Specifying and building cohorts
  , module Cohort

    -- ** Creating an executable cohort application
  , module Hasklepias.MakeApp

    -- * Statistical Types
  , module Stype

    -- * Rexported Functions and modules
  , module Hasklepias.Reexports
  , module Hasklepias.ReexportsUnsafe
  ) where

import           EventData

import           IntervalAlgebra
import           IntervalAlgebra.IntervalUtilities
import           IntervalAlgebra.PairedInterval

import           Features

import           Cohort

import           Hasklepias.FeatureEvents
import           Hasklepias.MakeApp
import           Hasklepias.Misc
import           Hasklepias.Reexports
import           Hasklepias.ReexportsUnsafe
import           Hasklepias.Templates

import           Stype
