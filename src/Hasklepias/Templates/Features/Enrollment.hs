{-|
Module      : Enrollment Features Templates 
Description : Templates for Features pertaining to enrollment
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Hasklepias.Templates.Features.Enrollment (
    defIsEnrolled 
  , defContinuousEnrollment
  , defEnrollmentTests
) where

import Control.Applicative              ( Applicative(..) )
import GHC.Int                          ( Int )
import GHC.TypeLits                     ( KnownSymbol )
import GHC.Show                         ( Show )
import Flow                             ( (|>), (.>) )
import IntervalAlgebra
import IntervalAlgebra.IntervalUtilities ( combineIntervals )
import IntervalAlgebra.PairedInterval   ( intervals )
import Witherable                       ( Witherable, filter )
import Data.Eq                          ( Eq )
import Data.Functor.Contravariant       ( Predicate(..) )
import Data.Foldable                    (Foldable(..), any)
import Data.Function                    ( ($), (.) )
import Data.Functor                     ( Functor(.. ) )
import Data.Maybe                       ( Maybe )
import Data.Monoid                      ( Monoid(..) )
import Data.Text                        ( Text )
import Data.Tuple                       ( uncurry )                     
import Test.Tasty                       ( testGroup, TestName, TestTree )
import Test.Tasty.HUnit                 ( testCase )

import EventData                        ( Event
                                        , Domain(..)
                                        , EnrollmentFacts(..)
                                        , event
                                        , context
                                        , isEnrollmentEvent
                                        )
import Features.Compose                 ( Feature
                                        , Definition(..)
                                        , Define(..)
                                        , Eval(..)
                                        , makeFeature )
import Hasklepias.FeatureEvents         ( allGapsWithinLessThanDuration
                                        )
import Hasklepias.Templates.TestUtilities
                                        ( makeAssertion
                                        , TemplateTestCase(..) )
import Hasklepias.Misc                  ( F )
import Cohort.Index                     ( Index, makeIndex )

import Cohort.AssessmentIntervals       ( makeBaselineFromIndex 
                                        , AssessmentInterval
                                        )
import Cohort.Criteria                  ( Status(..), includeIf )


{-| Is Enrolled

TODO: describe this

-}
defIsEnrolled ::
  ( Intervallic i0 a
  , Monoid (container (Interval a))
  , Applicative container
  , Witherable container) =>
  Predicate (Event a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
  (   Feature indexName  (Index i0 a)
   -> Feature eventsName (container (Event a))
   -> Feature varName     Status )
defIsEnrolled predicate =
  define
      (\index ->
           filter (getPredicate predicate)
        .> combineIntervals
        .> any (concur index)
        .> includeIf
      )

makeIsEnrolledTestInputs :: (IntervalSizeable a b) =>
     TestName
  -> b 
  -> a
  -> [Event a]
  -> Status
  -> TemplateTestCase (F "index" (Index Interval a), F "events" [Event a]) Status
makeIsEnrolledTestInputs name dur bgn e s = 
  MkTemplateTestCase name (pure (makeIndex $ beginerval dur bgn), pure e) (pure s)

makeEnrollmentEvent :: (IntervalSizeable a b) => b -> a -> Event a
makeEnrollmentEvent dur bgn = 
  event (beginerval dur bgn) (context ( Enrollment (EnrollmentFacts ())) mempty)

defIsEnrolledTestCases :: [TemplateTestCase
   (F "index" (Index Interval Int), F "events" [Event Int]) Status]
defIsEnrolledTestCases = [ 
      f "Exclude if no events" 1 (0::Int) []   Exclude
    , f "Exclude if only interval meets" 1 (0::Int) [g 5 1]  Exclude
    , f "Include if concurring interval" 1 (0::Int) [g 5 (-1)] Include
    , f "Include if concurring interval" 1 (0::Int) [g 2 (-1), g 5 1]  Include 
  ] where f = makeIsEnrolledTestInputs
          g = makeEnrollmentEvent

defIsEnrolledTests :: TestTree
defIsEnrolledTests = testGroup "Tests of isEnrolled template"
     ( fmap (\x -> testCase (getTestName x) (makeAssertion x (defIsEnrolled isEnrollmentEvent)) )
       defIsEnrolledTestCases )


{-| Continuous Enrollment 

TODO: describe this
-}
defContinuousEnrollment ::
  ( Monoid (container (Interval a))
  , Monoid (container (Maybe (Interval a)))
  , Applicative container
  , Witherable container
  , IntervalSizeable a b) =>
    (Index i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess enrollment
  -> Predicate (Event a)  -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> b  -- ^ duration of allowable gap between enrollment intervals
  -> Definition
  (   Feature indexName  (Index i0 a)
   -> Feature eventsName (container (Event a))
   -> Feature prevName    Status
   -> Feature varName     Status )
defContinuousEnrollment makeAssessmentInterval predicate allowableGap =
  define
    (\index events prevStatus ->
      case prevStatus of
        Exclude -> Exclude
        Include -> includeIf
          ( allGapsWithinLessThanDuration
                allowableGap
                (makeAssessmentInterval index)
                (combineIntervals $ filter (getPredicate predicate) events))
    )

makeContinuousEnrollmentTestInputs :: (IntervalSizeable a b) =>
     TestName
  -> b 
  -> a
  -> [Event a]
  -> Status
  -> Status
  -> TemplateTestCase (F "index" (Index Interval a), F "events" [Event a], F "prev" Status) Status
makeContinuousEnrollmentTestInputs name dur bgn e prev s = 
  MkTemplateTestCase name (pure (makeIndex $ beginerval dur bgn), pure e, pure prev) (pure s)

defContinuousEnrollmentTestCases :: [TemplateTestCase
   (F "index" (Index Interval Int), F "events" [Event Int], F "prev" Status) Status]
defContinuousEnrollmentTestCases = [ 
      f "Exclude if previously excluded" 1 (0::Int) []  Exclude  Exclude
    , f "Exclude if no events" 1 (0::Int) []  Include  Exclude
      {-
                  -           <- Index
         ----------           <- Baseline
         ---     ---          <- Enrollment
        |--------------|
      -}
    , f "Exclude if gap >= 3" 1 (10::Int) [g 3 1, g 3 9]  Include Exclude
      {-
                  -           <- Index
        ----------            <- Baseline
         ------               <- Enrollment
        |--------------|
      -}
    , f "Exclude if gap >= 3" 1 (10::Int) [g 6 1]   Include Exclude
        {-
                  -           <- Index
         ----------           <- Baseline
              -------         <- Enrollment
        |--------------|
      -}
    , f "Exclude if gap >= 3" 1 (10::Int) [g 7 6]   Include Exclude
      {-
                  -           <- Index
         ----------           <- Baseline
         --  -------          <- Enrollment
        |--------------|
      -}
    , f "Include if gaps less than 3" 1 (10::Int) [g 2 1, g 7 5]  Include Include
      {-
                  -           <- Index
         ----------           <- Baseline
          -------             <- Enrollment
        |--------------|
      -}
    , f "Include if gaps less than 3" 1 (10::Int) [g 7 2]  Include Include
        {-
                  -           <- Index
         ----------           <- Baseline
         -----                <- Enrollment
             ----
        |--------------|
      -}
    , f "Include if gaps less than 3" 1 (10::Int) [g 5 1, g 4 4]  Include Include
  ] where f = makeContinuousEnrollmentTestInputs
          g = makeEnrollmentEvent

defContinuousEnrollmentTests :: TestTree
defContinuousEnrollmentTests = testGroup "Tests of continuous enrollment template"
     ( fmap (\x -> testCase (getTestName x) 
          (makeAssertion x (defContinuousEnrollment (makeBaselineFromIndex 10) isEnrollmentEvent 3)) )
           defContinuousEnrollmentTestCases )

defEnrollmentTests :: TestTree
defEnrollmentTests = testGroup "" [defIsEnrolledTests, defContinuousEnrollmentTests]