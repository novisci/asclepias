{-|
Module      : Hasklepias Cohorts
Description : Defines the Cohort type and associated methods
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cohort(

   -- * Defining Cohorts
      module Cohort.Core

    -- ** Criteria
    , module Cohort.Criteria
    -- ** Index
    , module Cohort.Index

    -- * Cohort I/O
    -- ** Input
    , module Cohort.Input   
    -- ** Output
    , module Cohort.Output


) where


import Cohort.Core
import Cohort.Index
import Cohort.Criteria
import Cohort.Input
import Cohort.Output
