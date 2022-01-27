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

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , withObject
                                                )
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
import           IntervalAlgebra                ( beginerval )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )
import           Witch                          ( into )

-- | Just a dummy type with which to define an event
data SillyDomain =
    A Int
  | B Text
  | C
  | D
  deriving (Show, Eq, Generic)

instance FromJSON SillyDomain where
  parseJSON = withObject "Domain" $ \o -> do
    domain :: Text <- o .: "domain"
    case domain of
      "A" -> A <$> o .: "facts"
      "B" -> B <$> o .: "facts"
      "C" -> pure C
      "D" -> pure D
      _   -> fail ("unknown domain: " <> show domain)

-- | Just a dummy type to test non-text Concepts
data SillyConcepts = Mouse | Giraffe | Hornbill
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SillyConcepts

type SillyEvent1 a = Event SillyDomain Text a
type SillyEvent2 a = Event SillyDomain SillyConcepts a

c1 :: Context SillyDomain Text
c1 = Context { concepts = into (["this", "that"] :: [Text])
             , facts    = A 1
             , source   = Nothing
             }

e1 :: SillyEvent1 Int
e1 = event (beginerval 2 0) c1

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

cPred1 :: Predicate (Context SillyDomain Text)
cPred1 = Predicate (\x -> facts x == C)

cPred2 :: Predicate (Maybe Source)
cPred2 = Predicate isNothing

cPred3 :: Predicate SillyDomain
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
  "Unit test that pack/unpack concepts roundtrips"
  [ testCase "single concept" $ "foo" @?= (unpackConcept . packConcept) "foo"
  , testCase "multiple concepts"
  $   sort ["foo", "bar"]
  @?= (unpackConcepts . packConcepts) ["foo", "bar"]
  ]

-- | Check that files in test/events-day-text-good successfully parse
decodeSillyTests1 :: IO TestTree
decodeSillyTests1 =
  eventDecodeTests @SillyDomain @Text @Day "test/events-day-text-good"

-- | Check that files in test/events-day-text-bad successfully fail
decodeSillyFailTests1 :: IO TestTree
decodeSillyFailTests1 =
  eventDecodeFailTests @SillyDomain @Text @Day "test/events-day-text-bad"

-- | Check that files in test/events-integer-silly-good successfully parse
decodeSillyTests2 :: IO TestTree
decodeSillyTests2 = eventDecodeTests @SillyDomain @SillyConcepts @Integer
  "test/events-integer-silly-good"

-- | Check that files in test/events-integer-silly-bad successfully fail
decodeSillyFailTests2 :: IO TestTree
decodeSillyFailTests2 =
  eventDecodeFailTests @SillyDomain @Text @Day "test/events-integer-silly-bad"

theoryTests :: IO ()
theoryTests = defaultMain . testGroup "Event Theory tests" =<< sequenceA
  [ decodeSillyTests1
  , decodeSillyTests2
  , decodeSillyFailTests1
  , decodeSillyFailTests2
  , pure hasConceptUnitTests
  , pure eventPredicateUnitTests
  , pure toFromConceptsUnitTests
  ]

