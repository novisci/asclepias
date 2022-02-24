{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Test.Hygiea.ToOutput as TO
--import Examples.Simple
import Test.Hygiea.Map
import Test.Hygiea.Parse

main :: IO ()
main = do
  putStrLn "\nDhall shape from input_list.dhall for Csv\n"
  inputList <- parseDhallFile "./hygiea/src/Examples/input_list.dhall"
  print inputList
  let inputDecode = decodeMapSchemaAuto @TestAtomic inputList
  putStrLn "\nTestMap parsed from Csv with schema\n"
  recs <-  tryParseRecordsCsv inputDecode "./hygiea/src/Examples/input.csv"
  print recs
