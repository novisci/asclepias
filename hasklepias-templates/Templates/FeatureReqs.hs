{- Just a module containing imports used in Feature builders so only one module
   needs to be imported at the top of a feature builder module
-}
-- {-# OPTIONS_HADDOCK hide #-}

module Templates.FeatureReqs (
    module Cohort
  , module EventData
  , module Features
  , module Hasklepias.FeatureEvents
  , module Hasklepias.Reexports
  , module Hasklepias.ReexportsUnsafe
  , module Stype
  , module Templates.TestUtilities 
) where

import           Cohort
import           EventData
import           Features
import           Hasklepias.FeatureEvents
import           Hasklepias.Reexports
import           Hasklepias.ReexportsUnsafe
import           Stype
import           Templates.TestUtilities