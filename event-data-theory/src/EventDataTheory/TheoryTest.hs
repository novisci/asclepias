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

type SillyEvent a = Event SillyDomain Text a

c1 :: Context SillyDomain Text
c1 = Context { concepts = into (["this", "that"] :: [Text])
             , facts    = A 1
             , source   = Nothing
             }

e1 :: SillyEvent Int
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

-- | Check that files in test/events-good successfully parse
decodeSillyTests :: IO TestTree
decodeSillyTests = eventDecodeTests @SillyDomain @Text @Day "test/events-good"

-- | Check that files in test/events-bad successfully fail
decodeSillyFailTests :: IO TestTree
decodeSillyFailTests =
  eventDecodeFailTests @SillyDomain @Text @Day "test/events-bad"


theoryTests :: IO ()
theoryTests = defaultMain . testGroup "Event Theory tests" =<< sequenceA
  [ decodeSillyTests
  , decodeSillyFailTests
  , pure hasConceptUnitTests
  , pure eventPredicateUnitTests
  , pure toFromConceptsUnitTests
  ]

