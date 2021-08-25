{-|
Module      :  Features Templates 
Description : Templates for Features based on satisfying a set of predicates 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Hasklepias.Templates.Features.NsatisfyP
  ( buildNofX
  , buildNsatisfyPTests
  ) where

import           Control.Applicative            ( Applicative(..) )
import           GHC.Int                        ( Int )
import           GHC.Natural                    ( Natural
                                                , naturalToInt
                                                )

import           Data.Bool                      ( Bool(..) )
import           Data.Eq                        ( Eq )
import           Data.Foldable                  ( Foldable(..)
                                                , any
                                                , length
                                                )
import           Data.Function                  ( ($)
                                                , (.)
                                                , id
                                                )
import           Data.Functor                   ( Functor(..) )
import           Data.Functor.Contravariant     ( Predicate(..) )
import           Data.Maybe                     ( Maybe
                                                , fromMaybe
                                                , maybe
                                                )
import           Data.Monoid                    ( Monoid(..) )
import           Data.Ord                       ( Ord(..) )
import           Data.Text                      ( Text )
import           Data.Traversable               ( Traversable )
import           Data.Tuple                     ( uncurry )
import           Flow                           ( (.>)
                                                , (|>)
                                                )
import           GHC.Show                       ( Show )
import           GHC.TypeLits                   ( KnownSymbol )
import           IntervalAlgebra
import           IntervalAlgebra.IntervalUtilities
                                                -- ( combineIntervals )
import           IntervalAlgebra.PairedInterval ( intervals )
import           Test.Tasty                     ( TestName
                                                , TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( testCase )
import           Witherable                     ( Witherable
                                                , filter
                                                )

import           Cohort.Index                   ( Index
                                                , makeIndex
                                                )
import           EventData                      ( Domain(..)
                                                , EnrollmentFacts(..)
                                                , Event
                                                , context
                                                , event
                                                , isEnrollmentEvent
                                                , packConcepts
                                                )
import           EventData.Predicates
import           Features.Compose               ( Define(..)
                                                , Definition(..)
                                                , Eval(..)
                                                , Feature
                                                , makeFeature
                                                )
import           Hasklepias.FeatureEvents       ( allGapsWithinLessThanDuration
                                                )
import           Hasklepias.Misc                ( F )
import           Hasklepias.Templates.TestUtilities
                                                ( TemplateTestCase(..)
                                                , makeAssertion
                                                )

import           Cohort.AssessmentIntervals     ( AssessmentInterval
                                                , Baseline
                                                , makeBaselineFromIndex
                                                )
import           Cohort.Criteria                ( Status(..)
                                                , includeIf
                                                )
import           IntervalAlgebra.IntervalUtilities
                                                ( gaps )

buildNofXBase
  :: (Intervallic i0 a, Intervallic i1 a, Witherable container)
  => (container (i1 a) -> outputType) -- ^ function mapping the processed events to the output type
  -> (container (Event a) -> container (i1 a)) -- ^ function mapping a container of events to a container of intervallic intervals (which could be events!)
  -> (Index i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess the feature
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a) -- ^ the interval relation of the input events to the assessment interval
  -> Predicate (Event a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (container (Event a))
       -> Feature varName outputType
       )
buildNofXBase post process makeAssessmentInterval relation predicate = 
  define
    (\index ->
      filter (relation (makeAssessmentInterval index) &&& getPredicate predicate)
        .> process
        .> post
    )

{-| Do N events relating to the 'AssessmentInterval' in some way the satisfy 
    the given predicate? 
-}
buildNofX
  :: (Intervallic i0 a, Witherable t)
  => Natural
  -> (Index i0 a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Bool
       )
buildNofX n = buildNofXBase (\x -> length x >= naturalToInt n) id

getIntervalGaps
  :: ( Ord a
     , Applicative container
     , Witherable container
     , Monoid (container (Maybe (Interval a)))
     , Monoid (container (Interval a))
     )
  => container (Event a)
  -> container (Interval a)
getIntervalGaps es = fromMaybe mempty (gaps (fmap getInterval es))

{-| Do N events relating to the 'AssessmentInterval' in some way with at least
    a given gap between them the satisfy the given predicate? 
-}
buildNofXWithGap
  :: ( Intervallic i0 a
     , IntervalSizeable a b
     , Witherable t
     , Applicative t
     , Monoid (t (Maybe (Interval a)))
     , Monoid (t (Interval a))
     )
  => Natural
  -> b
  -> (Index i0 a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Bool
       )
buildNofXWithGap n allowableGap = buildNofXBase
  (\x -> length x >= naturalToInt n)
  (filter (\i -> duration i > allowableGap) . getIntervalGaps)

{-| Do N events concurring with baseline of a given duration satisfy the given
    predicate? 
-}
buildNofXConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a)
  => Natural
  -> b
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Bool
       )
buildNofXConcurBaseline n baselineDur =
  buildNofX n (makeBaselineFromIndex baselineDur) concur

{-| Do N events concurring with baseline of a given duration have a set of 
    concepts? 
-}
buildNofConceptsConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a)
  => Natural
  -> b
  -> [Text]
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Bool
       )
buildNofConceptsConcurBaseline n baselineDur cpts =
  buildNofX n (makeBaselineFromIndex baselineDur) concur (containsConcepts cpts)

makeNofXTestInputs
  :: (IntervalSizeable a b)
  => TestName
  -> b
  -> a
  -> [Event a]
  -> Bool
  -> TemplateTestCase
       (F "index" (Index Interval a), F "events" [Event a])
       Bool
makeNofXTestInputs name dur bgn e b = MkTemplateTestCase
  name
  (pure (makeIndex $ beginerval dur bgn), pure e)
  (pure b)

makeDummyEvent :: (IntervalSizeable a b) => b -> a -> Event a
makeDummyEvent dur bgn =
  event (beginerval dur bgn) (context (Enrollment (EnrollmentFacts ())) mempty)

makeEventWithConcepts :: (IntervalSizeable a b) => [Text] -> b -> a -> Event a
makeEventWithConcepts cpts dur bgn = event
  (beginerval dur bgn)
  (context (Enrollment (EnrollmentFacts ())) (packConcepts cpts))

type NofXArgs
  = ( Natural
    , Index Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2 (AssessmentInterval Int) (Event Int)
    , Predicate (Event Int)
    )
type NofXTestCase
  = TemplateTestCase
      (F "index" (Index Interval Int), F "events" [Event Int])
      Bool

buildNofXTestCases :: [(NofXTestCase, NofXArgs)]
buildNofXTestCases =
  [ ( f "False if no events" 1 (0 :: Int) [] False
    , (1, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
    )
  , ( f
      "False if 1 event after index but looking for single event concurring with baseline"
      1
      (0 :: Int)
      [g 5 2]
      False
    , (1, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
    )
  , ( f
      "True if 1 event before index and looking for single event concurring with baseline"
      1
      (0 :: Int)
      [h ["A", "B"] 1 (-5)]
      True
    , (1, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    )
    -- , f "1 event after index, 1 before index" 1 (0::Int) [g 1 (-2), g 5 2]
    -- , f "1 event after index, 2 before index but equal" 1 (0::Int) [g 2 (-1), g 2 (-1), g 5 1]
    -- , f "1 event after index, 2 before index" 1 (0::Int) [g 2 (-5), g 2 (-1), g 5 1]
  ] where
  f = makeNofXTestInputs
  g = makeDummyEvent
  h = makeEventWithConcepts

buildNofXTests :: TestTree
buildNofXTests = testGroup
  "Tests of NofX template"
  (fmap
    (\(x, (a, b, c, d)) ->
      testCase (getTestName x) (makeAssertion x (buildNofX a b c d))
    )
    buildNofXTestCases
  )

buildNsatisfyPTests :: TestTree
buildNsatisfyPTests = testGroup "NsatisfyP" 
  [buildNofXTests]
