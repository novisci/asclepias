{- Just a module containing imports used in Feature builders so only one module
   needs to be imported at the top of a feature builder module
-}
-- {-# OPTIONS_HADDOCK hide #-}

module Templates.FeatureReqs
  ( module Hasklepias.AssessmentIntervals
  , module Cohort
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Text
  , module Data.Time
  , module EventDataTheory
  , module Features
  , module Flow
  , module GHC.Natural
  , module Stype
  , module Templates.TestUtilities
  , module Test.Tasty
  , module W
  , naturalToInt
  ) where

import           Cohort
import           Data.Foldable                  ( toList )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           EventDataTheory
import           Features
import           Flow                           ( (.>) )
import           GHC.Natural                    ( Natural
                                                , naturalToInteger
                                                -- , naturalToInt
                                                )
import           Hasklepias.AssessmentIntervals
import           Stype
import           Templates.TestUtilities
import           Test.Tasty                     ( TestTree )
import           Witherable                    as W
                                                ( Witherable
                                                , filter
                                                )

-- FIXME:
-- There is not From Natural Int instance,
-- nor From Integer Int instance.
-- There are TryFrom instance, since the casts are unsafe.
-- For now, providing this utility,
-- even though it's not ideal.
-- Currently used itn hasklepias-templates.
naturalToInt :: Natural -> Int
naturalToInt = fromInteger . naturalToInteger
