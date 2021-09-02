{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Cohort.OutputSpec (
  spec
 ) where

import Cohort
import Data.Aeson
import Data.Set (fromList)
import Test.Hspec ( describe, pending, shouldBe, it, Spec )

attr1 :: Maybe AttritionInfo
attr1 = 
  Just $ MkAttritionInfo 2 $
    fromList [ uncurry MkAttritionLevel (ExcludedBy (1, "feat2"), 1)
             , uncurry MkAttritionLevel (Included, 1)]

spec :: Spec
spec = do

  describe "Cohort I/O" $
    do

      it "AttritionInfo can roundtrip via JSON" $
        decode (encode attr1) `shouldBe` attr1

