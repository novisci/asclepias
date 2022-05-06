-- TODO: this modules needs to be redone

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



spec :: Spec
spec = do


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
