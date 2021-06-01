{-# LANGUAGE OverloadedStrings #-}
module EventData.ContextSpec (spec) where

import EventData.Context as HC
import Test.Hspec ( shouldBe, it, Spec )
import Data.Set(fromList)

ctxt1 :: Context
ctxt1 = HC.context $ packConcepts  ["c1", "c2"]

ctxt2 :: Context
ctxt2 = HC.context $ packConcepts ["c2", "c3"]

spec :: Spec
spec = do
    it "getConcepts returns correct values" $
       _concepts ctxt1 `shouldBe` packConcepts ["c1", "c2"]
    it "hasConcept returns True when concept is in context" $
      (ctxt1 `hasConcept` "c1") `shouldBe` True
    it "hasConcept returns False when concept is not in context" $
      (ctxt1 `hasConcept` "c3") `shouldBe` False
    it "hasConcepts returns True when at at least one concept is in context" $
      (ctxt1 `hasConcepts` ["c3", "c1"]) `shouldBe` True
    it "hasConcepts returns False when no concept is in context" $
      (ctxt1 `hasConcepts` ["c3", "c4"]) `shouldBe` False

    it "<> combines contexts" $
      (ctxt1 <> ctxt2) `shouldBe` HC.context (packConcepts ["c1", "c2", "c3"])