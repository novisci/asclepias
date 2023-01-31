{-# LANGUAGE OverloadedStrings #-}
module Stype.Tests.Censored
  ( tests
  ) where

import           Stype
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup
  "basic censored tests"
  [ testCase "parseIntervalCensor returns error on invalid interval"
  $   parseIntervalCensor (mkEventTime (Just 5)) (mkEventTime (Just 1))
  @?= Left
        (ParseIntervalError
          "EventTime {getEventTime = NonNegCont 1} < EventTime {getEventTime = NonNegCont 5}"
        )
  , testCase "parseIntervalCensor returns right on valid interval"
  $   parseIntervalCensor (mkEventTime (Just 5.0)) (mkEventTime (Just 9.0))
  @?= Right
        (IntervalCensored (EventTime (NonNegCont 5.0))
                          (EventTime (NonNegCont 9.0))
        )
  ]
