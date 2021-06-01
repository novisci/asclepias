{-# LANGUAGE OverloadedStrings #-}

module FeatureCompose.CriteriaSpec (spec) where

import FeatureCompose
import FeatureCompose.Criteria
import Test.Hspec ( describe, pending, shouldBe, it, Spec )

f1 :: Feature String Bool
f1 = MkFeature "f1" "" (featureDataR [True, False, True])

f2 :: Feature String Bool
f2 = MkFeature "f2" "" (featureDataR [True, True, True])

f3 :: Feature String Bool
f3 = MkFeature "f3" "" (featureDataR [True, True, True])

spec :: Spec
spec = do

  describe "checking d1" $
    do

      it "include f2/include f3" $
        runCriteria (MkCriteria [include f2, include f3]) `shouldBe`
             featureDataR [Included, Included, Included ]

      it "exclude f2/include f3" $
        runCriteria (MkCriteria [exclude f2, include f3]) `shouldBe`
             featureDataR [ExcludedBy 1, ExcludedBy 1, ExcludedBy 1]

      it "include f2/exclude f3" $
        runCriteria (MkCriteria [include f2, exclude f3]) `shouldBe`
             featureDataR [ExcludedBy 2, ExcludedBy 2, ExcludedBy 2]

      it "exclude f3/exclude f3" $
        runCriteria (MkCriteria [exclude f2, exclude f3]) `shouldBe`
             featureDataR [ExcludedBy 1, ExcludedBy 1, ExcludedBy 1]

      it "include f1/exclude f3" $
        runCriteria (MkCriteria [include f1, exclude f2]) `shouldBe`
             featureDataR [ExcludedBy 2, ExcludedBy 1, ExcludedBy 2]

      -- it "" $
      --   runCriteria (MkCriteria [ include (MkFeature "f1" "" (featureDataR []))])
      --     `shouldBe` featureDataL Excluded