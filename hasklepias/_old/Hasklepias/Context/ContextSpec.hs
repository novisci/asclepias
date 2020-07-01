module Hasklepias.Context.ContextSpec where

import Hasklepias.Context
import Hasklepias.Context.ClaimsDomain
import Test.Hspec

dx = domain $ Diagnosis Inpatient (Code "W61.62" ICD10) Nothing Nothing
ii = domain $ Insurance "hmo" "ACME Insurance" 


main :: IO ()
main = hspec $ do
  describe "Tests of Contexts and Domains" $ 
    do 
      it "A simple unit test to get the ball rolling" $ 
         (code $ get dxCode dx) `shouldBe` "W61.62"
