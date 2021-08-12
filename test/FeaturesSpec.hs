{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module FeaturesSpec (
 spec
) where

import Features
import Test.Hspec ( describe, pending, shouldBe, it, Spec )


-----------------------------------
-- example :: Feature "test" ()
-- example = MkFeature $ pure ()

-- d0 :: Definition (FeatureData Int)
-- d0 = define 5

d1 :: Definition (FeatureData Int -> FeatureData Int)
d1 = defineA
  (\x ->
    if x < 0 then
      missingBecause $ Other "at least 1 < 0"
    else
      pure (x + 1)
   )

d2 :: Definition (FeatureData Int -> FeatureData Int)
d2 = define (*2)


d3 ::  Definition (FeatureData Int -> FeatureData Int -> FeatureData Int)
d3 = define (+)

f1 :: Int -> Int
f1 = (+2)

f1D :: Definition (FeatureData Int -> FeatureData Int)
f1D = define f1

f1F :: Definition (Feature "someInt" Int -> Feature "anotherInt" Int)
f1F = define f1

f2 :: Bool -> FeatureData Int
f2 True = pure 1
f2 False = missingBecause $ Other "test"

f2D :: Definition (FeatureData Bool -> FeatureData Int)
f2D = defineA f2

f2' :: Bool -> Feature "someInt" Int
f2' True = pure 1
f2' False = makeFeature $ missingBecause $ Other "test"

f2F :: Definition (Feature "someBool" Bool -> Feature "someInt" Int)
f2F = defineA f2'

f3 :: Bool -> Int -> String
f3 True 1 = "this"
f3 False 9 = "that"
f3 _ _ = "otherwise"

f3D :: Definition (FeatureData Bool -> FeatureData Int -> FeatureData String)
f3D = define f3

f3F :: Definition (Feature "myBool" Bool -> Feature "someInt" Int -> Feature "myString" String)
f3F = define f3


featInts :: [Int] -> Feature "someInts" [Int]
featInts = pure

feat1 :: Definition (Feature "someInts" [Int] -> Feature "hasMoreThan3" Bool)
feat1 = defineA
  (\ints -> if null ints then makeFeature (missingBecause $ Other "no data")
           else makeFeature $ featureDataR (length ints > 3))

feat2 :: Definition (
      Feature "hasMoreThan3" Bool
  -> Feature "someInts" [Int]
  -> Feature "sum" Int)
feat2 = define (\b ints -> if b then sum ints else 0)

ex0 = featInts []
ex0a = eval feat1 ex0 -- MkFeature (MkFeatureData (Left (Other "no data")))
ex0b = eval feat2 (ex0a, ex0) -- MkFeature (MkFeatureData (Left (Other "no data")))

ex1 = featInts [3, 8]
ex1a = eval feat1 ex1 -- MkFeature (MkFeatureData (Right False))
ex1b = eval feat2 (ex1a, ex1) -- MkFeature (MkFeatureData (Right 0))

ex2 = featInts [1..4]
ex2a = eval feat1 ex2 -- MkFeature (MkFeatureData (Right True))
ex2b = eval feat2 (ex2a, ex2) -- MkFeature (MkFeatureData (Right 10))

spec :: Spec
spec = do

  describe "checking d1" $
    do
      it "eval of d1 on d0" $
        eval d1 (pure 5) `shouldBe` featureDataR  6
      it "eval of d1 returns correct value" $
        eval d1 (featureDataR 5 ) `shouldBe`
          featureDataR 6
      it "d1 returns correct error" $
        eval d1 (featureDataR (-1)) `shouldBe`
          missingBecause (Other "at least 1 < 0")

  describe "checking d2" $
    do
      it "eval of d2 returns correct value" $
        eval d2 (featureDataR 5) `shouldBe`
          featureDataR 10
      it "eval of d2 returns correct value" $
        eval d2 (featureDataR 5) `shouldBe`
          featureDataR 10
      it "d2 returns correct error" $
        eval d2 (featureDataL (Other "at least 1 < 0") ) `shouldBe`
          featureDataL (Other "at least 1 < 0")

  describe "checking d3" $
    do
      it "eval of d2 returns correct values" $
        eval d3 (featureDataR 5, featureDataR 6)  `shouldBe`
          featureDataR 11
      it "eval of d3 returns correct values" $
        eval d3 (featureDataR 5, featureDataR 5) `shouldBe`
          featureDataR 10
      it "d3 returns correct error" $
        eval d3 (featureDataL (Other "at least 1 < 0"), featureDataR 6)  `shouldBe`
          featureDataL (Other "at least 1 < 0")
      it "d3 returns correct error" $
        eval d3 (featureDataR 6, featureDataL (Other "at least 1 < 0")) `shouldBe`
          featureDataL (Other "at least 1 < 0")

  describe "checking f1" $
    do
      it "eval of f1F on d0 returns correct value" $
        eval f1F (pure 5) `shouldBe` pure 7

  describe "checking f2" $
    do
      it "eval of f2D on d0 returns correct value" $
        eval f2D (pure True) `shouldBe` pure 1
      it "eval of f1F on d0 returns correct value" $
        eval f2D (pure False) `shouldBe` missingBecause (Other "test")
      it "eval of f1F on d0 returns correct value" $
        eval f2F (pure False) `shouldBe` makeFeature (missingBecause (Other "test"))

  describe "checking f3" $
    do
      it "eval of f3D returns correct value" $
        eval f3D (pure True, pure 1) `shouldBe` pure "this"
      it "eval of f3D returns correct value" $
        eval f3D (pure True, pure 9) `shouldBe` pure "otherwise"
      it "eval of f3F returns correct value" $
        eval f3F (pure True, eval f2F (pure False)) `shouldBe` makeFeature (missingBecause (Other "test"))
      it "eval of f3F  returns correct value" $
        eval f3F (pure True, eval f2F (pure True)) `shouldBe` pure "this"

  describe "checking ex0-2" $
    do
      it "ex0b" $
        ex0b `shouldBe` makeFeature (missingBecause (Other "no data"))
      it "ex1b" $
        ex1b `shouldBe` makeFeature (featureDataR 0)
      it "ex2b" $
        ex2b `shouldBe` makeFeature (featureDataR 10)