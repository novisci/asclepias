{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module FeaturesSpec
  ( spec
  ) where

import           Features
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , pending
                                                , shouldBe
                                                )


-----------------------------------

f1 :: Int -> Int
f1 = (+ 2)

f1F :: Definition (Feature "someInt" Int -> Feature "anotherInt" Int)
f1F = define f1

f2 :: Bool -> FeatureData Int
f2 True  = pure 1
f2 False = missingBecause $ Other "test"

f2' :: Bool -> Feature "someInt" Int
f2' True  = pure 1
f2' False = makeFeature $ missingBecause $ Other "test"

f2F :: Definition (Feature "someBool" Bool -> Feature "someInt" Int)
f2F = defineA f2'

f3 :: Bool -> Int -> String
f3 True  1 = "this"
f3 False 9 = "that"
f3 _     _ = "otherwise"

f3F
  :: Definition
       (  Feature "myBool" Bool
       -> Feature "someInt" Int
       -> Feature "myString" String
       )
f3F = define f3


featInts :: [Int] -> Feature "someInts" [Int]
featInts = pure 

feat1 :: Definition (Feature "someInts" [Int] -> Feature "hasMoreThan3" Bool)
feat1 = defineA
  (\ints -> if null ints
    then makeFeature (missingBecause $ Other "no data")
    else makeFeature $ featureDataR (length ints > 3)
  )

feat2
  :: Definition
       (  Feature "hasMoreThan3" Bool
       -> Feature "someInts" [Int]
       -> Feature "sum" Int
       )
feat2 = define (\b ints -> if b then sum ints else 0)

ex0 = featInts []
ex0a = eval feat1 ex0 -- MkFeature (MkFeatureData (Left (Other "no data")))
ex0b = eval feat2 ex0a ex0 -- MkFeature (MkFeatureData (Left (Other "no data")))

ex1 = featInts [3, 8]
ex1a = eval feat1 ex1 -- MkFeature (MkFeatureData (Right False))
ex1b = eval feat2 ex1a ex1 -- MkFeature (MkFeatureData (Right 0))

ex2 = featInts [1 .. 4]
ex2a = eval feat1 ex2 -- MkFeature (MkFeatureData (Right True))
ex2b = eval feat2 ex2a ex2 -- MkFeature (MkFeatureData (Right 10))

spec :: Spec
spec = do

  describe "checking f1" $ do
    it "eval of f1F on d0 returns correct value"
      $          eval f1F (pure 5)
      `shouldBe` pure 7

  describe "checking f2" $ do
    it "eval of f1F on d0 returns correct value"
      $          eval f2F (pure False)
      `shouldBe` makeFeature (missingBecause (Other "test"))

  describe "checking f3" $ do
    it "eval of f3F returns correct value"
      $          eval f3F (pure True) (eval f2F (pure False))
      `shouldBe` makeFeature (missingBecause (Other "test"))
    it "eval of f3F  returns correct value"
      $          eval f3F (pure True) (eval f2F (pure True))
      `shouldBe` pure "this"

  describe "checking ex0-2" $ do
    it "ex0b" $ ex0b `shouldBe` makeFeature (missingBecause (Other "no data"))
    it "ex1b" $ ex1b `shouldBe` makeFeature (featureDataR 0)
    it "ex2b" $ ex2b `shouldBe` makeFeature (featureDataR 10)
