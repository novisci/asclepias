{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : event data theory tests
-- Description : An internal module for testing event data theory functions
--               on a dummy event data model
module EventDataTheory.TheoryTest
  ( theoryTests,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Data
import Data.Functor.Contravariant (Predicate (..))
import Data.List (sort)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import EventDataTheory.Core
import EventDataTheory.EventLines
import EventDataTheory.Test
import EventDataTheory.Utilities
import GHC.Generics (Generic)
import GHC.Num (Natural)
import IntervalAlgebra
  ( beginerval,
    filterContains,
    meets,
    metBy,
    overlaps,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Witch (from, into)

-- | Just a dummy type with which to define an event

{- tag::exampleEvent[] -}
data SillySchema
  = A Int
  | B Text
  | C
  | D
  deriving (Show, Eq, Generic, Data)

instance FromJSON SillySchema

type SillyEvent1 a = Event Text SillySchema a

{- end::exampleEvent[] -}

instance ToJSON SillySchema

-- | Just a dummy type to test non-text tag set
data SillyTagSet = Mouse | Giraffe | Hornbill
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SillyTagSet

type SillyEvent2 a = Event SillyTagSet SillySchema a

c1 :: Context Text SillySchema
c1 = context (into (["this", "that"] :: [Text])) (A 1) Nothing

e1 :: SillyEvent1 Int
e1 = event (beginerval 2 1) c1

c2 :: Context Text SillySchema
c2 = context (into (["this", "another"] :: [Text])) (A 1) Nothing

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
eventIntervalUnitTests =
  testGroup
    "Interval algebra sanity checks"
    [ testCase "e1 meets e2" $ meets e1 e2 @?= True,
      testCase "e2 metBy e1" $ metBy e2 e1 @?= True,
      testCase "e1 does not overlap e2" $ overlaps e1 e2 @?= False,
      testCase "(0, 10) contains both e1 and e2" $ filterContains ci es @?= es,
      testCase "(4, 10) contains neither e1 and e2" $ filterContains ni es @?= []
    ]
  where
    es = [e1, e2]
    ci = beginerval 10 0
    ni = beginerval 6 4

{-
Tests of the hasTagSet functions.
-}
hasTagUnitTests :: TestTree
hasTagUnitTests =
  testGroup
    "Unit tests for hasTagSet using a dummy event model"
    [ testCase "hasTag should have tag" $ hasTag e1 ("this" :: Text) @?= True,
      testCase "hasTag should not have tag" $ hasTag e1 ("not" :: Text) @?= False,
      testCase "hasAnyTag works" $ hasAnyTag e1 (["this"] :: [Text]) @?= True,
      testCase "hasAnyTags works" $ hasAnyTag e1 (["not"] :: [Text]) @?= False,
      testCase "hasAnyTags works" $
        hasAnyTag e1 (["not", "this"] :: [Text])
          @?= True,
      testCase "hasAllTags works" $
        hasAllTags e1 (["not", "this"] :: [Text])
          @?= False,
      testCase "hasAllTags works" $
        hasAllTags e1 (["that", "this"] :: [Text])
          @?= True,
      testCase "hasAllTags works" $
        hasAllTags e1 (["that", "this", "not"] :: [Text])
          @?= False
    ]

cPred1 :: Predicate (Context Text SillySchema)
cPred1 = Predicate (\x -> getFacts x == C)

cPred2 :: Predicate (Maybe Source)
cPred2 = Predicate isNothing

cPred3 :: Predicate SillySchema
cPred3 = Predicate (A 1 ==)

eventPredicateUnitTests :: TestTree
eventPredicateUnitTests =
  testGroup
    "Unit tests that predicate on event components successfully lift"
    [ testCase "Context" $
        getPredicate cPred1 c1
          @?= getPredicate (liftToEventPredicate cPred1) e1,
      testCase "Maybe Source" $
        getPredicate cPred2 Nothing
          @?= getPredicate (liftToEventPredicate cPred2) e1,
      testCase "Facts" $
        getPredicate cPred3 C
          @?= not
            (getPredicate (liftToEventPredicate cPred3) e1),
      testCase "Facts" $
        getPredicate cPred3 (A 1)
          @?= getPredicate (liftToEventPredicate cPred3) e1
    ]

toFromTagSetUnitTests :: TestTree
toFromTagSetUnitTests =
  testGroup
    "Unit test that pack/unpack getTagSet roundtrips"
    [ testCase "single tag" $ "foo" @?= (unpackTag . packTag) "foo",
      testCase "tag set" $
        sort ["foo", "bar"]
          @?= (unpackTagSet . packTagSet)
            ["foo", "bar"]
    ]

-- | Check that files in test/events-day-text-good successfully parse
decodeSillyTests1 :: IO TestTree
decodeSillyTests1 =
  eventDecodeTests @SillySchema @Text @Day "test/events-day-text-good"

-- | Check that files in test/events-day-text-good successfully parse
roundtripSillyTests1 :: IO TestTree
roundtripSillyTests1 =
  eventLineRoundTripTests @SillySchema @Text @Day "test/events-day-text-good"

-- | Check that files in test/events-day-text-good successfully parse
modifySillyTests1 :: IO TestTree
modifySillyTests1 =
  eventLineModifyTests @SillySchema @Text @Day "test/events-day-text-good"

-- | Check that files in test/events-day-text-bad successfully fail
decodeSillyFailTests1 :: IO TestTree
decodeSillyFailTests1 =
  eventDecodeFailTests @SillySchema @Text @Day "test/events-day-text-bad"

-- | Check that files in test/events-integer-silly-good successfully parse
decodeSillyTests2 :: IO TestTree
decodeSillyTests2 =
  eventDecodeTests @SillySchema @SillyTagSet @Integer
    "test/events-integer-silly-good"

-- | Check that files in test/events-integer-silly-bad successfully fail
decodeSillyFailTests2 :: IO TestTree
decodeSillyFailTests2 =
  eventDecodeFailTests @SillySchema @Text @Day "test/events-integer-silly-bad"

{- Unit tests on line parsers -}
testInput1Good :: B.ByteString
testInput1Good =
  "[\"abc\", \"2020-01-01\", \"2020-01-02\", \"A\",\
  \[\"someThing\"],\
  \{\"facts\" : {\"tag\":\"A\", \"contents\" : 1},\
  \ \"patient_id\":\"abc\",\
  \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]"

testInput2Good :: B.ByteString
testInput2Good =
  "[\"abc\", \"2020-01-05\", \"2020-01-06\", \"C\",\
  \[\"someThing\"],\
  \{\"facts\": { \"tag\" : \"C\", \"contents\":{}},\
  \ \"patient_id\":\"abc\",\
  \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testInput1Bad :: B.ByteString
testInput1Bad =
  "[\"def\", \"2020-01-01\", null, \"D\",\
  \[\"someThing\"],\
  \{\"facts\": { \"tag\" : \"D\", \"contents\":{}},\
  \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]"

testInput2Bad :: B.ByteString
testInput2Bad =
  "[\"def\", \"2020-01-05\", null, \"C\",\
  \[\"someThing\"],\
  \ {\"facts\":{\"tag\":\"C\", \"contents\":{}},\
  \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testInputBad :: B.ByteString
testInputBad = testInput1Bad <> "\n" <> testInput2Bad

testOutput1Good, testOutput2Good, testOutputBad :: ([LineParseError], [(SubjectID, Event Text SillySchema Day)])
testOutput1Good = ([], [("abc", event (beginerval 1 (fromGregorian 2020 1 1)) (context (into ["someThing" :: Text]) (A 1) Nothing))])
testOutput2Good = ([], [("abc", event (beginerval 2 (fromGregorian 2020 1 5)) (context (into ["someThing" :: Text]) C Nothing))])
testOutputBad =
  ( [ from @(Natural, String) (1, "Error in $[5]: parsing EventDataTheory.EventLines.FactsLine(MkFactsLine) failed, key \"patient_id\" not found"),
      from @(Natural, String) (2, "Error in $[5]: parsing EventDataTheory.EventLines.FactsLine(MkFactsLine) failed, key \"patient_id\" not found")
    ],
    []
  )

parserUnitTests :: TestTree
parserUnitTests =
  testGroup
    "Unit tests of EventLines parsers"
    [ testCase "with valid inputs 1" $
        parseEventLinesL @SillySchema @Text @Day
          defaultParseEventLineOption
          testInput1Good
          @?= testOutput1Good,
      testCase "with valid inputs 2" $
        parseEventLinesL @SillySchema @Text @Day
          defaultParseEventLineOption
          testInput2Good
          @?= testOutput2Good,
      testCase "with invalid inputs" $
        parseEventLinesL @SillySchema @Text @Day
          defaultParseEventLineOption
          testInputBad
          @?= testOutputBad
    ]

-- | Unit tests on Core utilities
singleEventGoodIn :: B.ByteString
singleEventGoodIn =
  "[\"abc\",\"2020-01-01\",\"2020-01-02\",\"A\",\
  \[],\
  \{\"facts\":{\"contents\":1,\"tag\":\"A\"},\
  \\"patient_id\":\"abc\",\
  \\"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-02\"}}]"

singleEventGoodOut :: B.ByteString
singleEventGoodOut =
  "[\"abc\",\"2020-01-01\",\"2020-01-02\",\"A\",\
  \[\"bar\",\"foo\"],\
  \{\"facts\":{\"contents\":1,\"tag\":\"A\"},\
  \\"patient_id\":\"abc\",\
  \\"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-02\"}}]"

testAddTagViaEventLine :: IO ()
testAddTagViaEventLine =
  let x =
        modifyEventLineWithContext @SillySchema @SillySchema @Text @Text @Day
          defaultParseEventLineOption
          (liftToContextFunction $ addTagSet ["foo", "bar" :: Text])
          singleEventGoodIn
   in case x of
        Left s -> assertFailure s
        Right e ->
          decode (encode e)
            @?= decode @(EventLine Text SillySchema Day) singleEventGoodOut

-- | Unit tests on utilities
coreUtilitiesUnitTests :: TestTree
coreUtilitiesUnitTests =
  testGroup
    "Unit tests on Core utilities"
    [testCase "check that tag set is added as expected" testAddTagViaEventLine]

-- | Unit tests on utilities
utilitiesUnitTests :: TestTree
utilitiesUnitTests =
  testGroup
    "Unit tests on utilities"
    [ testCase "find first occurrence of Tag 'this'" $
        firstOccurrenceOfTag ["this"] [e1, e2]
          @?= Just e1,
      testCase "find last occurrence of Tag 'this'" $
        lastOccurrenceOfTag ["this"] [e1, e2]
          @?= Just e2,
      testCase "find first occurrence of Tag 'another'" $
        firstOccurrenceOfTag ["another"] [e1, e2]
          @?= Just e2,
      testCase "find first occurrence of Tag 'blah'" $
        firstOccurrenceOfTag ["blah"] [e1, e2]
          @?= Nothing
    ]

-- TODO: rewrite this test group. decode tests need not have nested IO.

-- |
-- The set of tests used to test the @event-data-theory@ package.
theoryTests :: IO ()
theoryTests =
  defaultMain . testGroup "Event Theory tests"
    =<< sequenceA
      [ decodeSillyTests1,
        decodeSillyTests2,
        decodeSillyFailTests1,
        decodeSillyFailTests2,
        roundtripSillyTests1,
        modifySillyTests1,
        pure coreUtilitiesUnitTests,
        pure eventIntervalUnitTests,
        pure hasTagUnitTests,
        pure eventPredicateUnitTests,
        pure toFromTagSetUnitTests,
        pure utilitiesUnitTests,
        pure parserUnitTests,
        pure eventOrdTests
      ]
