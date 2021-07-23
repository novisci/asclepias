{-|
Module      : ExampleCohortApp
Description : Demostrates how to define a cohort using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main(
  main
) where
import Hasklepias

{-------------------------------------------------------------------------------
  Features used by inclusion/exclusion (and possibly other places too)
-------------------------------------------------------------------------------}

-- | Lift a subject's events in a feature
featureEvents :: Events Day -> Feature "allEvents" (Events Day)
featureEvents = pure

-- | Lift a subject's events in a feature
featureDummy :: Definition
   ( Feature "allEvents" (Events Day)
   -> Feature "dummy" Bool)
featureDummy = define $ pure True

-- | Include the subject if she has an enrollment interval concurring with index.
critTrue :: Definition
   ( Feature "allEvents" (Events Day)
   -> Feature "dummy" Status)
critTrue = define $ pure Include 

instance HasAttributes "dummy" Bool where
  getAttributes _ = MkAttributes "Text" "Text" "Text"

{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}

-- | Make a function that runs the criteria
makeCriteriaRunner :: Events Day -> Criteria
makeCriteriaRunner events =
  criteria $ pure (criterion crit1)
  where crit1   = eval critTrue featEvs
        featEvs = featureEvents events

-- | Define the shape of features for a cohort
type CohortFeatures = [Featureable]
  -- (Feature "dummy"  Bool )


-- | Make a function that runs the features for a calendar index
makeFeatureRunner ::
       Events Day
    -> CohortFeatures
makeFeatureRunner events = [packFeature ( eval featureDummy ef )]
    where ef  = featureEvents events

-- | Make a cohort specification for each calendar time
cohortSpecs :: [CohortSpec (Events Day) CohortFeatures]
cohortSpecs = [specifyCohort makeCriteriaRunner makeFeatureRunner]

main :: IO ()
main = makeCohortApp "testCohort" "v0.1.0" cohortSpecs
