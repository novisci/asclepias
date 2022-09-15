{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Features.Core
  ( tests
  ) where

import           Features.Core
import           Test.Tasty
import           Test.Tasty.HUnit

f1 :: Int -> Int
f1 = (+ 2)

f1F :: Definition (Feature "someInt" Int -> Feature "anotherInt" Int)
f1F = define f1

f2 :: Bool -> FeatureData Int
f2 True  = pure 1
f2 False = missingBecause $ CustomFlag "test"

f2' :: Bool -> Feature "someInt" Int
f2' True  = pure 1
f2' False = makeFeature $ missingBecause $ CustomFlag "test"

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
    then makeFeature (missingBecause $ CustomFlag "no data")
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
ex0a = eval feat1 ex0 -- MkFeature (MkFeatureData (Left (CustomFlag  "no data")))
ex0b = eval feat2 ex0a ex0 -- MkFeature (MkFeatureData (Left (CustomFlag  "no data")))

ex1 = featInts [3, 8]
ex1a = eval feat1 ex1 -- MkFeature (MkFeatureData (Right False))
ex1b = eval feat2 ex1a ex1 -- MkFeature (MkFeatureData (Right 0))

ex2 = featInts [1 .. 4]
ex2a = eval feat1 ex2 -- MkFeature (MkFeatureData (Right True))
ex2b = eval feat2 ex2a ex2 -- MkFeature (MkFeatureData (Right 10))

tests :: TestTree
tests = testGroup
  "Unit tests on Features module"
  [ testCase "eval of f1F on d0 returns correct value"
  $   eval f1F (pure 5)
  @?= pure 7
  , testCase "eval of f1F on d0 returns correct value"
  $   eval f2F (pure False)
  @?= makeFeature (missingBecause (CustomFlag "test"))
  , testCase "eval of f3F returns correct value"
  $   eval f3F (pure True) (eval f2F (pure False))
  @?= makeFeature (missingBecause (CustomFlag "test"))
  , testCase "eval of f3F  returns correct value"
  $   eval f3F (pure True) (eval f2F (pure True))
  @?= pure "this"
  , testCase "checking ex0-2 with ex0b" $ ex0b @?= makeFeature
    (missingBecause (CustomFlag "no data"))
  , testCase "checking ex0-2 with ex1b" $ ex1b @?= makeFeature (featureDataR 0)
  , testCase "checking ex0-2 with ex1b" $ ex2b @?= makeFeature (featureDataR 10)
  ]
