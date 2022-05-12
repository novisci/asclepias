{- TODO: This is purely for demonstration and should be deleted once the
    library is used in, say, hasklepias-examples. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Examples.Simple
import           Test.Monarch.Parse
--import Examples.Simple
import           Test.Monarch.TestMap
import qualified Test.Monarch.ToOutput          as TO
import           Test.Tasty
import           Test.Tasty.Monarch
import           Witch.TryFrom

tests :: TestTree
tests = testGroup
  "Examples.Simple"
  [ monarchTest "good" myRoutine
  , monarchTest "bad"  myBadRoutine
  , monarchTest "ugly" myMisspecRoutine
  ]

main :: IO ()
main = do
  {- UNCOMMENT TO INSPECT INTERNAL BEHAVIOR -}
    {-
  putStrLn "\nDhall shape from input.dhall for Csv\n"
  input <- parseDhallFile "./monarch/src/Examples/input.dhall"
  print input

  let inputDecode = decodeMapSchemaAuto @TestVal input

  putStrLn "\nTestMap parsed from Csv with schema\n"
  recs <- tryParseRecordsCsv inputDecode "./monarch/src/Examples/input.csv"
  print recs

  putStrLn "\ntryFrom @[TestMap] @[ProjEvent]\n"
  print $ fmap (tryFrom @[TestMap] @[ProjEvent]) recs

  putStrLn "\nOops. User supplied Text in Integer type column\n"
  recs <- tryParseRecordsCsv inputDecode "./monarch/src/Examples/input_bad.csv"
  print recs
-}

  putStrLn
    "\n-------\n-------\nRunning tests in Example using Test.Tasty.Monarch\n-------\n-------\n"

  putStrLn "\nFirst test should pass: We filter out only the last event.\n"
  putStrLn "\nSecond test should fail: Output includes only first event.\n"
  putStrLn "\nThird test should crash: Input csv is misspecified.\n"
  defaultMain tests
