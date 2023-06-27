{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Data
import Data.Functor.Contravariant (Predicate (..))
import Data.List (sort)
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import Data.Time (Day, fromGregorian)
import EventDataTheory.Core
import EventDataTheory.EventLines
import EventDataTheory.Test
import EventDataTheory.Utilities
import GHC.Generics (Generic)
import GHC.Num (Natural)
import IntervalAlgebra
  ( Interval,
    Intervallic (..),
    Iv,
    PointedIv (..),
    SizedIv (..),
    begin,
    beginerval,
    contains,
    end,
    expandr,
    meets,
    metBy,
    momentize,
    overlaps,
  )
import IntervalAlgebra.Arbitrary
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Witch (from, into)

-- TODO: assess whether this needs to be provided as a utility
-- in event-data-theory. it was previously only used here.
-- Note Ord a implies Iv (Interval a).

-- | Temporary stand-in for a utility that has been removed from
-- interval-algebra. The slightly odd type signature is there to match
-- the old API and usage here.
filterContains :: (Ord a, Intervallic i) => Interval a -> [i a] -> [i a]
filterContains iv = filter (iv `contains`)

-- | Just a dummy type with which to define an event

{- tag::exampleEvent[] -}
data SillySchema
  = A Int
  | B Text
  | C
  | D
  deriving (Show, Eq, Generic, Data)

instance FromJSON SillySchema

instance Arbitrary SillySchema where
  arbitrary =
    oneof
      [ pure C,
        pure D,
        A <$> arbitrary,
        B . pack <$> arbitrary
      ]

type SillyEvent1 a = Event String SillySchema a

{- end::exampleEvent[] -}

instance ToJSON SillySchema

-- | Just a dummy type to test non-text tag set
data SillyTagSet = Mouse | Giraffe | Hornbill
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SillyTagSet

instance ToJSON SillyTagSet

instance Arbitrary SillyTagSet where
  arbitrary = oneof [pure Mouse, pure Giraffe, pure Hornbill]

type SillyEvent2 a = Event SillyTagSet SillySchema a

-- TODO: remove these if they can still just alias `arbitrary`
-- Generators for SillyEvents
genSillyEvent1 :: (Arbitrary (Interval a)) => Gen (SillyEvent1 a)
genSillyEvent1 = arbitrary

genSillyEvent2 :: (Arbitrary (Interval a)) => Gen (SillyEvent2 a)
genSillyEvent2 = arbitrary

-- Examples for SillyEvents

c1 :: Context String SillySchema
c1 = context (from @[String] ["this", "that"]) (A 1) Nothing

e1 :: SillyEvent1 Int
e1 = event (beginerval 2 1) c1

c2 :: Context String SillySchema
c2 = context (from @[String] ["this", "another"]) (A 1) Nothing

e2 :: SillyEvent1 Int
e2 = event (beginerval 4 3) c2

{- UNIT TESTS -}

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
    [ testCase "hasTag should have tag" $ hasTag e1 ("this" :: String) @?= True,
      testCase "hasTag should not have tag" $ hasTag e1 ("not" :: String) @?= False,
      testCase "hasAnyTag works" $ hasAnyTag e1 (["this"] :: [String]) @?= True,
      testCase "hasAnyTags works" $ hasAnyTag e1 (["not"] :: [String]) @?= False,
      testCase "hasAnyTags works" $
        hasAnyTag e1 (["not", "this"] :: [String])
          @?= True,
      testCase "hasAllTags works" $
        hasAllTags e1 (["not", "this"] :: [String])
          @?= False,
      testCase "hasAllTags works" $
        hasAllTags e1 (["that", "this"] :: [String])
          @?= True,
      testCase "hasAllTags works" $
        hasAllTags e1 (["that", "this", "not"] :: [String])
          @?= False
    ]

cPred1 :: Predicate (Context String SillySchema)
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
decodeSillyTests1 :: TestTree
decodeSillyTests1 =
  eventDecodeTests @SillySchema @Text @Day "test/events-day-text-good"

-- | Check that files in test/events-day-text-good successfully parse
roundtripSillyTests1 :: TestTree
roundtripSillyTests1 =
  eventLineRoundTripTests @SillySchema @Text @Day "test/events-day-text-good"

-- | Check that files in test/events-day-text-good successfully parse
modifySillyTests1 :: TestTree
modifySillyTests1 =
  eventLineModifyTests @SillySchema @Text @Day "test/events-day-text-good"

-- | Check that files in test/events-day-text-bad successfully fail
decodeSillyFailTests1 :: TestTree
decodeSillyFailTests1 =
  eventDecodeFailTests @SillySchema @Text @Day "test/events-day-text-bad"

-- | Check that files in test/events-integer-silly-good successfully parse
decodeSillyTests2 :: TestTree
decodeSillyTests2 =
  eventDecodeTests @SillySchema @SillyTagSet @Integer
    "test/events-integer-silly-good"

-- | Check that files in test/events-integer-silly-bad successfully fail
decodeSillyFailTests2 :: TestTree
decodeSillyFailTests2 =
  eventDecodeFailTests @SillySchema @Text @Day "test/events-integer-silly-bad"

{- Unit tests on line parsers -}
testInput1Good :: C.ByteString
testInput1Good =
  "[\"abc\", \"2020-01-01\", \"2020-01-02\", \"A\",\
  \[\"someThing\"],\
  \{\"facts\" : {\"tag\":\"A\", \"contents\" : 1},\
  \ \"patient_id\":\"abc\",\
  \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]"

testInput2Good :: C.ByteString
testInput2Good =
  "[\"abc\", \"2020-01-05\", \"2020-01-06\", \"C\",\
  \[\"someThing\"],\
  \{\"facts\": { \"tag\" : \"C\", \"contents\":{}},\
  \ \"patient_id\":\"abc\",\
  \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testInput1Bad :: C.ByteString
testInput1Bad =
  "[\"def\", \"2020-01-01\", null, \"D\",\
  \[\"someThing\"],\
  \{\"facts\": { \"tag\" : \"D\", \"contents\":{}},\
  \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]"

testInput2Bad :: C.ByteString
testInput2Bad =
  "[\"def\", \"2020-01-05\", null, \"C\",\
  \[\"someThing\"],\
  \ {\"facts\":{\"tag\":\"C\", \"contents\":{}},\
  \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testInputBad :: C.ByteString
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
        parseEventLinesL' @SillySchema @Text @Day
          AddMomentAndFix
          testInput1Good
          @?= testOutput1Good,
      testCase "with valid inputs 2" $
        parseEventLinesL' @SillySchema @Text @Day
          AddMomentAndFix
          testInput2Good
          @?= testOutput2Good,
      testCase "with invalid inputs" $
        parseEventLinesL' @SillySchema @Text @Day
          AddMomentAndFix
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

testAddTagViaEventLine :: Assertion
testAddTagViaEventLine =
  let x =
        modifyEventLineWithContext @SillySchema @SillySchema @Text @Text @Day
          AddMomentAndFix
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

-- | Test group
theoryUnitTests :: TestTree
theoryUnitTests =
  testGroup
    "Event Theory unit tests"
    [ decodeSillyTests1,
      decodeSillyTests2,
      decodeSillyFailTests1,
      decodeSillyFailTests2,
      roundtripSillyTests1,
      modifySillyTests1,
      coreUtilitiesUnitTests,
      eventIntervalUnitTests,
      hasTagUnitTests,
      eventPredicateUnitTests,
      toFromTagSetUnitTests,
      utilitiesUnitTests,
      parserUnitTests,
      eventOrdTests
    ]

{- PROPERTY TESTS -}

-- TODO: revise these constraints when you do so for parseEventLinesL'

-- | Utility to parse a Bytestring eventline and grab the
-- correctly parsed events, ignoring the rest. Uses the specified
-- parsing option.
parsedEvents ::
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a) =>
  ParseEventLineOption ->
  C.ByteString ->
  [Event t m a]
parsedEvents opt = map snd . snd . parseEventLinesL' opt

-- TODO: modify this to support creating begin == end intervals.

-- | Utility to wrap an event into a single eventline, with the provided Text as subject ID.
-- The 'modFact' input allows to modify the FactLine before parsing.
eventToEventLine ::
  (Eventable t m a, EventLineAble t m a b) =>
  (FactsLine m a -> FactsLine m a) -> 
  Text ->
  Event t m a ->
  EventLine t m a
eventToEventLine modFact sid e = MkEventLine Null Null Null Null tgs $ modFact factline
  where
    factline =
      MkFactsLine
        { valid = Nothing,
          time =
            MkTimeLine
              { timeEnd = Just (ivEnd i),
                timeBegin = ivBegin i
              },
          source = getSource ctx,
          patient_id = sid,
          facts = getFacts ctx
        }
    ctx = getContext e
    i = getInterval e
    tgs = into $ getTagSet ctx

-- | Utility to set the timeEnd in a 'FactLine'.
setTimeEnd :: Maybe a -> FactsLine m a -> FactsLine m a
setTimeEnd e f = f
      { time = MkTimeLine {timeEnd = e, timeBegin = timeBegin $ time f}
      }

-- | Transform a list of events into EventLines, then encode as a single bytestring,
-- with one eventline per line. The Bool flag if True sets timeEnd to Nothing
encodeEventList ::
  (EventLineAble t m a b, Eventable t m a, ToJSON t, ToJSON m, ToJSON a) =>
  (FactsLine m a -> FactsLine m a) -> 
  [Event t m a] ->
  C.ByteString
encodeEventList modFact es = C.intercalate "\n" bs
  where
    bs = zipWith op [1 ..] es
    op t e = C.toStrict $ encode $ eventToEventLine modFact (pack $ show t) e

-- | Valid events should round-trip through JSON with the DoNotModifyTime
-- parsing option.
prop_noModTimeOption :: [SillyEvent2 Int] -> Property
prop_noModTimeOption es = es === es'
  where
    es' = parsedEvents DoNotModifyTime $ encodeEventList id es

-- | AddMomentToEnd works as advertised for valid events.
prop_addMoment :: [SillyEvent2 Int] -> Property
prop_addMoment es = esExpanded === es'
  where
    m = moment @(Interval Int)
    esExpanded = map (expandr m) es
    es' = parsedEvents AddMomentToEnd $ encodeEventList id es

-- | AddMomentAndFix creates a moment-length interval when
-- only a 'begin' is provided.
prop_addMomentToPoint :: [SillyEvent2 Int] -> Property
prop_addMomentToPoint es = esMoment === es'
  where
    esMoment = map momentize es
    es' = parsedEvents AddMomentAndFix $ encodeEventList (setTimeEnd Nothing) es

-- | AddMomentToEnd does *not* fixup missing end, and no such events should parse.
prop_addMomentMissingEnd :: [SillyEvent2 Int] -> Property
prop_addMomentMissingEnd es = es' === ([] :: [SillyEvent2 Int])
  where es' = parsedEvents AddMomentToEnd $ encodeEventList (setTimeEnd Nothing) es

-- | AddMomentToEnd works as advertised for events where begin == end.
prop_addMomentToPoint' :: [SillyEvent2 Int] -> Property
prop_addMomentToPoint' es = esPoint === es'
  where
    esPoint = map momentize es
    es' = parsedEvents AddMomentToEnd $ encodeEventList (\f -> setTimeEnd (Just $ timeBegin $ time f) f) es

-- | FixEnd creates a moment-length interval when
-- only a 'begin' is provided and otherwise leaves events untouched.
-- Note this does *not* check whether it fails when it should.
prop_FixEnd :: [SillyEvent2 Int] -> Property
prop_FixEnd es = (es ++ esMoment) === (es' ++ es'')
  where
    esMoment = map momentize es
    es'' = parsedEvents FixEnd $ encodeEventList (setTimeEnd Nothing) es
    es' = parsedEvents FixEnd $ encodeEventList id es

-- | Test group
theoryPropTests :: TestTree
theoryPropTests =
  testGroup
    "Event Data Theory property tests"
    [ testProperty
        "DoNotModifyTime gives JSON roundtrip with valid events"
        prop_noModTimeOption,
      testProperty
        "AddMomentToEnd gives JSON roundtrip re: valid events expanded rightward by moment"
        prop_addMoment,
      testProperty
        "AddMomentToEnd does not fix up missing end"
        prop_addMomentMissingEnd,
      testProperty
        "AddMomentAndFix gives JSON roundtrip for eventlines with no end point"
        prop_addMomentToPoint,
      testProperty
        "AddMomentToEnd gives JSON roundtrip for eventlines where end == begin"
        prop_addMomentToPoint',
      testProperty
        "FixEnd gives JSON roundtrip for eventlines with no end point, leaving rest untouched"
        prop_FixEnd
    ]

{- TEST RUNNER -}

-- |
-- The set of tests used to test the @event-data-theory@ package.
theoryTests :: IO ()
theoryTests =
  defaultMain $
    testGroup
      "Event Data Theory tests"
      [ theoryUnitTests,
        theoryPropTests
      ]
