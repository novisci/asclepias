{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Test.Hygiea.ToOutput as TO
--import Examples.Simple
import Test.Hygiea.Map
import Test.Hygiea.Parse

main :: IO ()
main = do
  putStrLn "\nDhall shape from input.dhall for Csv\n"
  input <- parseDhallFile "./hygiea/src/Examples/input.dhall"
  print input

  let inputDecode = decodeMapSchemaAuto @TestAtomic input

  putStrLn "\nTestMap parsed from Csv with schema\n"
  recs <-  tryParseRecordsCsv inputDecode "./hygiea/src/Examples/input.csv"
  print recs

  putStrLn "\nOops. User supplied negative number for a Natural type column\n"
  recs <-  tryParseRecordsCsv inputDecode "./hygiea/src/Examples/input_bad.csv"
  print recs
