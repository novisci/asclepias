{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Hasklepias.FeatureEventsSpec
  ( spec
  ) where

import           Data.Maybe
import           Data.Time.Calendar             ( fromGregorian )
import           EventData                      ( Event
                                                , event
                                                )
import           EventData.Context             as HC
                                                ( context
                                                , packConcepts
                                                )
import           EventData.Domain
import           Hasklepias.FeatureEvents
import           IntervalAlgebra
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )

-- | Toy events for unit tests
evnt1 :: Event Int
evnt1 = event
  (beginerval (4 :: Int) (1 :: Int))
  (HC.context (UnimplementedDomain ()) (packConcepts ["c1", "c2"]) Nothing)
evnt2 :: Event Int
evnt2 = event
  (beginerval (4 :: Int) (2 :: Int))
  (HC.context (UnimplementedDomain ()) (packConcepts ["c3", "c4"]) Nothing)
evnts :: [Event Int]
evnts = [evnt1, evnt2]

spec :: Spec
spec = do
  it "find first occurrence of c1"
    $          firstConceptOccurrence ["c1"] evnts
    `shouldBe` Just evnt1
  it "find first occurrence of c3"
    $          firstConceptOccurrence ["c3"] evnts
    `shouldBe` Just evnt2
  it "find first occurrence of c5"
    $          firstConceptOccurrence ["c5"] evnts
    `shouldBe` Nothing

  it "yearFromDay" $ yearFromDay (fromGregorian 2021 8 18) `shouldBe` 2021
  it "monthFromDay" $ monthFromDay (fromGregorian 2021 8 18) `shouldBe` 8
  it "dayOfMonthFromDay"
    $          dayOfMonthFromDay (fromGregorian 2021 8 18)
    `shouldBe` 18

  it "pairGaps" $ pairGaps ([] :: [Interval Int]) `shouldBe` []
  it "pairGaps"
    $          pairGaps [beginerval 5 (0 :: Int), beginerval 5 (0 :: Int)]
    `shouldBe` [Nothing]
  it "pairGaps"
    $          pairGaps
                 [ beginerval 5 (0 :: Int)
                 , beginerval 5 (0 :: Int)
                 , beginerval 1 (10 :: Int)
                 ]
    `shouldBe` [Nothing, Just 5, Just 5]
  it "pairGaps"
    $          pairGaps
                 [ beginerval 5 (0 :: Int)
                 , beginerval 1 (6 :: Int)
                 , beginerval 1 (10 :: Int)
                 ]
    `shouldBe` [Just 1, Just 5, Just 3]
