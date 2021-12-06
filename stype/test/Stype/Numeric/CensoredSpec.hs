{-# LANGUAGE OverloadedStrings #-}
module Stype.Numeric.CensoredSpec
  ( spec
  ) where

import           Stype
import           Test.Hspec

spec :: Spec
spec = do
  describe "basic censored tests" $ do
    it "parseIntervalCensor returns error on invalid interval"
      $ parseIntervalCensor (mkEventTime (Just 5)) (mkEventTime (Just 1))
      `shouldBe` Left
                   (ParseIntervalError
                     "EventTime {getEventTime = NonNegCont 1} < EventTime {getEventTime = NonNegCont 5}"
                   )

    it "parseIntervalCensor returns right on valid interval"
      $ parseIntervalCensor (mkEventTime (Just 5.0)) (mkEventTime (Just 9.0))
      `shouldBe` Right
                   (IntervalCensored (EventTime (NonNegCont 5.0))
                                     (EventTime (NonNegCont 9.0))
                   )
