{-|
Module      : event data theory tests
Description : An internal module for testing event data theory functions 
              on a dummy event data model
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EventDataTheory.TheoryTest
  ( theoryTests
  ) where

import           Data.Aeson
import           Data.Functor.Contravariant     ( Predicate(..) )
import           Data.List                      ( sort )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           EventDataTheory.Core
import           EventDataTheory.Test           ( eventDecodeFailTests
                                                , eventDecodeTests
                                                )
import           GHC.Generics                   ( Generic )
import           IntervalAlgebra                ( beginerval
                                                , filterContains
                                                , meets
                                                , metBy
                                                , overlaps
                                                )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )
import           Witch                          ( into )

-- | Just a dummy type with which to define an event
{- tag::exampleEvent[] -}
data SillySchema =
    A Int
  | B Text
  | C
  | D
  deriving (Show, Eq, Generic)

instance FromJSON SillySchema where
  parseJSON = genericParseJSON
    (defaultOptions
      { sumEncoding = TaggedObject { tagFieldName      = "domain"
                                   , contentsFieldName = "facts"
                                   }
      }
    )

type SillyEvent1 a = Event SillySchema Text a
{- end::exampleEvent[] -}

-- | Just a dummy type to test non-text Concepts
data SillyConcepts = Mouse | Giraffe | Hornbill
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SillyConcepts

type SillyEvent2 a = Event SillySchema SillyConcepts a

c1 :: Context SillySchema Text
c1 = MkContext { getConcepts = into (["this", "that"] :: [Text])
             , getFacts    = A 1
             , getSource   = Nothing
             }

e1 :: SillyEvent1 Int
e1 = event (beginerval 2 1) c1

c2 :: Context SillySchema Text
c2 = MkContext { getConcepts = into (["this", "another"] :: [Text])
             , getFacts    = A 1
             , getSource   = Nothing
             }

e2 :: SillyEvent1 Int
e2 = event (beginerval 4 3) c2

{-
These tests of the interval algebra are in a way silly
because events are basically PairedIntervals
which are well tested in the interval-algebra library
These few tests are here for a basic sanity check
to be sure interval functions work on events.
-}
eventIntervalUnitTests :: TestTree
eventIntervalUnitTests = testGroup
  "Interval algebra sanity checks"
  [ testCase "e1 meets e2" $ meets e1 e2 @?= True
  , testCase "e2 metBy e1" $ metBy e2 e1 @?= True
  , testCase "e1 does not overlap e2" $ overlaps e1 e2 @?= False
  , testCase "(0, 10) contains both e1 and e2" $ filterContains ci es @?= es
  , testCase "(4, 10) contains neither e1 and e2" $ filterContains ni es @?= []
  ]
 where
  es = [e1, e2]
  ci = beginerval 10 0
  ni = beginerval 6 4

{-
Tests of the hasConcepts functions.
-}
hasConceptUnitTests :: TestTree
hasConceptUnitTests = testGroup
  "Unit tests for HasConcepts using a dummy event model"
  [ testCase "hasConcept should have concept"
  $   hasConcept e1 ("this" :: Text)
  @?= True
  , testCase "hasConcept should not have concept"
  $   hasConcept e1 ("not" :: Text)
  @?= False
  , testCase "haAnyConcept works"
  $   hasAnyConcepts e1 (["this"] :: [Text])
  @?= True
  , testCase "haAnyConcepts works"
  $   hasAnyConcepts e1 (["not"] :: [Text])
  @?= False
  , testCase "haAnyConcepts works"
  $   hasAnyConcepts e1 (["not", "this"] :: [Text])
  @?= True
  , testCase "haAllConcepts works"
  $   hasAllConcepts e1 (["not", "this"] :: [Text])
  @?= False
  , testCase "haAllConcepts works"
  $   hasAllConcepts e1 (["that", "this"] :: [Text])
  @?= True
  , testCase "haAllConcepts works"
  $   hasAllConcepts e1 (["that", "this", "not"] :: [Text])
  @?= False
  ]

cPred1 :: Predicate (Context SillySchema Text)
cPred1 = Predicate (\x -> getFacts x == C)

cPred2 :: Predicate (Maybe Source)
cPred2 = Predicate isNothing

cPred3 :: Predicate SillySchema
cPred3 = Predicate (A 1 ==)

eventPredicateUnitTests :: TestTree
eventPredicateUnitTests = testGroup
  "Unit tests that predicate on event components successfully lift"
  [ testCase "Context"
  $   getPredicate cPred1                        c1
  @?= getPredicate (liftToEventPredicate cPred1) e1
  , testCase "Maybe Source"
  $   getPredicate cPred2                        Nothing
  @?= getPredicate (liftToEventPredicate cPred2) e1
  , testCase "Facts" $ getPredicate cPred3 C @?= not
    (getPredicate (liftToEventPredicate cPred3) e1)
  , testCase "Facts"
  $   getPredicate cPred3                        (A 1)
  @?= getPredicate (liftToEventPredicate cPred3) e1
  ]

toFromConceptsUnitTests :: TestTree
toFromConceptsUnitTests = testGroup
  "Unit test that pack/unpack getConcepts roundtrips"
  [ testCase "single concept" $ "foo" @?= (unpackConcept . packConcept) "foo"
  , testCase "multiple concepts"
  $   sort ["foo", "bar"]
  @?= (unpackConcepts . packConcepts) ["foo", "bar"]
  ]

-- | Check that files in test/events-day-text-good successfully parse
decodeSillyTests1 :: IO TestTree
decodeSillyTests1 =
  eventDecodeTests @SillySchema @Text @Day "test/events-day-text-good"

-- | Check that files in test/events-day-text-bad successfully fail
decodeSillyFailTests1 :: IO TestTree
decodeSillyFailTests1 =
  eventDecodeFailTests @SillySchema @Text @Day "test/events-day-text-bad"

-- | Check that files in test/events-integer-silly-good successfully parse
decodeSillyTests2 :: IO TestTree
decodeSillyTests2 = eventDecodeTests @SillySchema @SillyConcepts @Integer
  "test/events-integer-silly-good"

-- | Check that files in test/events-integer-silly-bad successfully fail
decodeSillyFailTests2 :: IO TestTree
decodeSillyFailTests2 =
  eventDecodeFailTests @SillySchema @Text @Day "test/events-integer-silly-bad"

{-|
The set of tests used to test the @event-data-theory@ package.
-}
theoryTests :: IO ()
theoryTests = defaultMain . testGroup "Event Theory tests" =<< sequenceA
  [ decodeSillyTests1
  , decodeSillyTests2
  , decodeSillyFailTests1
  , decodeSillyFailTests2
  , pure eventIntervalUnitTests
  , pure hasConceptUnitTests
  , pure eventPredicateUnitTests
  , pure toFromConceptsUnitTests
  ]

