{-# LANGUAGE OverloadedStrings #-}
module EventData.ContextSpec
  ( spec
  ) where

import           Data.Maybe                     ( Maybe(Nothing) )
import           Data.Set                       ( fromList )
import           EventData.Context             as HC
import           EventData.Context.Domain
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )

ctxt1 :: Context
ctxt1 = HC.context (UnimplementedDomain ()) (packConcepts ["c1", "c2"]) Nothing

ctxt2 :: Context
ctxt2 = HC.context (UnimplementedDomain ()) (packConcepts ["c2", "c3"]) Nothing

spec :: Spec
spec = do
  it "getConcepts pures correct values" $ concepts ctxt1 `shouldBe` packConcepts
    ["c1", "c2"]
  it "hasConcept pures True when concept is in context"
    $          (ctxt1 `hasConcept` "c1")
    `shouldBe` True
  it "hasConcept pures False when concept is not in context"
    $          (ctxt1 `hasConcept` "c3")
    `shouldBe` False
  it "hasConcepts pures True when at at least one concept is in context"
    $          (ctxt1 `hasConcepts` ["c3", "c1"])
    `shouldBe` True
  it "hasConcepts pures False when no concept is in context"
    $          (ctxt1 `hasConcepts` ["c3", "c4"])
    `shouldBe` False
