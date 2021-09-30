{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hasklepias.ExampleApp
  ( exampleAppRW
  , exampleAppCW
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
makeCriteriaRunner :: Index Interval Day -> Events Day -> Criteria
makeCriteriaRunner _ events = criteria $ pure (criterion crit1)
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

exampleAppRW :: CohortApp IO
exampleAppRW = makeCohortApp "testCohort" "v0.1.0" rowWise cohortSpecs

exampleAppCW :: CohortApp IO
exampleAppCW = makeCohortApp "testCohort" "v0.1.0" colWise cohortSpecs 