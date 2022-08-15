{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module AppExamples.CohortApp
  ( exampleAppRW
  , exampleAppCW
  ) where
import           ExampleEvents (ExampleModel)
import           Hasklepias
{-------------------------------------------------------------------------------
  Features used by inclusion/exclusion (and possibly other places too)
-------------------------------------------------------------------------------}

-- | Lift a subject's events in a feature
featureEvents
  :: [Event Text ExampleModel Day]
  -> Feature "allEvents" [Event Text ExampleModel Day]
featureEvents = pure

-- | Lift a subject's events in a feature
featureDummy
  :: Definition
       (  Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature "myVar1" Count
       )
featureDummy = define $ pure 5

-- | Lift a subject's events in a feature
anotherDummy
  :: Bool
  -> Definition
       (  Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature "myVar2" Bool
       )
anotherDummy x = define $ const x

-- | Include the subject if she has an enrollment interval concurring with index.
critTrue
  :: Definition
       (  Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature "dummy" Status
       )
critTrue = define $ pure Include

instance HasAttributes "myVar1" Count where
  getAttributes = MkAttributes
    { getShortLabel = "somelabel"
    , getLongLabel  = "another label"
    , getDerivation = ""
    , getPurpose    = MkPurpose (into [Outcome]) (from @[Text] [])
    }

instance HasAttributes "myVar2" Bool where
  getAttributes = emptyAttributes
{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}

-- |
makeIndexRunner :: [Event Text ExampleModel Day] -> IndexSet (Interval Day)
makeIndexRunner _ = makeIndexSet [beginerval 1 (fromGregorian 2010 7 6)]

-- | Make a function that runs the criteria
makeCriteriaRunner :: Interval Day -> [Event Text ExampleModel Day] -> Criteria
makeCriteriaRunner _ events = criteria $ pure (into @Criterion crit1)
 where
  crit1   = eval critTrue featEvs
  featEvs = featureEvents events

-- | Make a function that runs the features for a calendar index
makeFeatureRunner
  :: Interval Day -> [Event Text ExampleModel Day] -> Featureset
makeFeatureRunner _ events = featureset
  (  packFeature (eval featureDummy ef)
  :| [packFeature (eval (anotherDummy True) ef)]
  )
  where ef = featureEvents events

-- | Make a cohort specification set
-- cohortSpecs :: CohortSetSpec [Event Text ExampleModel Day] Featureset (Interval Day)
cohortSpecs
  :: CohortMapSpec [Event Text ExampleModel Day] Featureset (Interval Day)
cohortSpecs = makeCohortSpecs
  [("example", makeIndexRunner, makeCriteriaRunner, makeFeatureRunner)]

exampleAppRW :: CohortApp IO
exampleAppRW = makeCohortApp "testCohort" "v0.1.0" rowWise cohortSpecs

exampleAppCW :: CohortApp IO
exampleAppCW = makeCohortApp "testCohort" "v0.1.0" colWise cohortSpecs
