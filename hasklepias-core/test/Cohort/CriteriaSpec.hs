{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Cohort.CriteriaSpec (
  spec
 ) where

import Features
import Cohort.Criteria
import Cohort.Index
import IntervalAlgebra
import Test.Hspec ( describe, pending, shouldBe, it, Spec )
import Data.List.NonEmpty


f1 :: Status -> Criterion 
f1 s = criterion (makeFeature (featureDataR s) :: Feature "f1" Status)

f2 :: Status -> Criterion 
f2 s = criterion (makeFeature  (featureDataR s) :: Feature "f2" Status)

f3 :: Status -> Criterion 
f3 s = criterion (makeFeature (featureDataR s) :: Feature "f3" Status)

f4 :: Criterion 
f4 = criterion ( makeFeature (featureDataL $ Other "something") :: Feature "f4" Status)

index :: Maybe (Index Interval Int)
index = Just $ makeIndex (beginerval 1 1)

spec :: Spec
spec = do

  describe "checking d1" $
    do

      it "noIndex " $
          checkCohortStatus Nothing (criteria $ pure (f1 Include)) `shouldBe` NoIndex

      it "include f1" $
          checkCohortStatus index (criteria $ pure (f1 Include)) `shouldBe` Included

      it "include f1, f2, f3" $
          checkCohortStatus index (criteria $ f1 Include :| [f2 Include, f3 Include] ) 
            `shouldBe` Included

      it "exclude on f2" $
          checkCohortStatus index (criteria $ f2 Exclude :| [f3 Include]) 
            `shouldBe` ExcludedBy (1, "f2")

      it "exclude on f2" $
          checkCohortStatus index (criteria $ f1 Include :| [f2 Exclude, f3 Include]) 
            `shouldBe` ExcludedBy (2, "f2")

      it "error on f4" $
          checkCohortStatus index (criteria $ f1 Include :| [f2 Include, f3 Include, f4]) 
            `shouldBe` ExcludedBy (4, "f4")
