{-|
Module      : ExampleCohortApp
Description : Demostrates how to define a cohort using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

To run as an example: 
cat hasklepias-main/exampleApp/exampleData.jsonl| cabal exec exampleApp
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main
  ( main
  ) where
import           Hasklepias

{-------------------------------------------------------------------------------
  Features used by inclusion/exclusion (and possibly other places too)
-------------------------------------------------------------------------------}

-- | Lift a subject's events in a feature
featureEvents :: Events Day -> Feature "allEvents" (Events Day)
featureEvents = pure

-- | Lift a subject's events in a feature
featureDummy
  :: Definition (Feature "allEvents" (Events Day) -> Feature "myVar1" Count)
featureDummy = define $ pure 5

-- | Lift a subject's events in a feature
anotherDummy
  :: Bool
  -> Definition (Feature "allEvents" (Events Day) -> Feature "myVar2" Bool)
anotherDummy x = define $ const x

-- | Include the subject if she has an enrollment interval concurring with index.
critTrue
  :: Definition (Feature "allEvents" (Events Day) -> Feature "dummy" Status)
critTrue = define $ pure Include

instance HasAttributes "myVar1" Count where
  getAttributes _ = 
    MkAttributes {
      getShortLabel = "somelabel"
    , getLongLabel  = "another label"
    , getDerivation = ""
    , getPurpose = MkPurpose (setFromList [Outcome]) (setFromList [])}

instance HasAttributes "myVar2" Bool where
  getAttributes _ = emptyAttributes
{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}

-- | 
makeIndexRunner :: Events Day -> IndexSet Interval Day
makeIndexRunner _ = MkIndexSet (Just $ fromList [makeIndex $ beginerval 1 (fromGregorian 2010 7 6)]) 

-- | Make a function that runs the criteria
makeCriteriaRunner :: Events Day -> Criteria
makeCriteriaRunner events = criteria $ pure (criterion crit1)
 where
  crit1   = eval critTrue featEvs
  featEvs = featureEvents events

-- | Make a function that runs the features for a calendar index
makeFeatureRunner :: Index Interval Day -> Events Day -> Featureset
makeFeatureRunner _ events = featureset
  (  packFeature (eval featureDummy ef)
  :| [packFeature (eval (anotherDummy True) ef)]
  )
  where ef = featureEvents events

-- | Make a cohort specification set
cohortSpecs :: CohortSetSpec (Events Day) Featureset Interval Day
cohortSpecs = 
  makeCohortSpecs [("example", makeIndexRunner, makeCriteriaRunner, makeFeatureRunner)]

main :: IO ()
main = runApp $ makeCohortApp "testCohort" "v0.1.0" rowWise cohortSpecs
