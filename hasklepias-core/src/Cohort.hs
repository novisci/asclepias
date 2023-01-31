{-|
Module      : Hasklepias Cohorts
Description : Defines the CohortSpecMap and supporting types, along
              with core cohort-evaluation logic.
Copyright   : (c) Target RWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com 
              ljackman@targetrwe.com 
              dpritchard@targetrwe.com
-}

module Cohort
  (
   -- ** Types for Cohort data
  module Cohort.Cohort

   -- ** Index
  , module Cohort.IndexSet

   -- ** Criteria
  , module Cohort.Criteria

  ) where

import           Cohort.Cohort   (CohortSpec (..), CohortSpecMap)
import           Cohort.Criteria (Criterion (..), Status (..), excludeIf,
                                  includeIf)
import           Cohort.IndexSet
