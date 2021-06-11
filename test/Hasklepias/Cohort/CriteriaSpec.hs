{-# LANGUAGE OverloadedStrings #-}

module Hasklepias.Cohort.CriteriaSpec (
  spec
 ) where

import FeatureCompose
import Hasklepias.Cohort.Criteria
import Test.Hspec ( describe, pending, shouldBe, it, Spec )
import Data.List.NonEmpty
f1 :: Status -> Criterion String
f1 s = criterion $ MkFeature "f1" "" (featureDataR s)

f2 :: Status -> Criterion String
f2 s = criterion $ MkFeature "f2" "" (featureDataR s)

f3 :: Status -> Criterion String
f3 s = criterion $ MkFeature "f3" "" (featureDataR s)

f4 :: Criterion String
f4 = criterion $ MkFeature "f4" "" (featureDataL $ Other "something")

spec :: Spec
spec = do

  describe "checking d1" $
    do

      it "include f1" $
          checkCohortStatus (criteria $ pure (f1 Include)) `shouldBe` Included

      it "include f1, f2, f3" $
          checkCohortStatus (criteria $ f1 Include :| [f2 Include, f3 Include] ) 
            `shouldBe` Included

      it "exclude on f2" $
          checkCohortStatus (criteria $ f2 Exclude :| [f3 Include]) 
            `shouldBe` ExcludedBy (1, "f2")

      it "exclude on f2" $
          checkCohortStatus (criteria $ f1 Include :| [f2 Exclude, f3 Include]) 
            `shouldBe` ExcludedBy (2, "f2")

      it "error on f4" $
          checkCohortStatus (criteria $ f1 Include :| [f2 Include, f3 Include, f4]) 
            `shouldBe` ExcludedBy (4, "f4")
