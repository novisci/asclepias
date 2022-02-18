{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Planning.Test as T
import Planning.Output
import Planning.Decode
import Planning.Examples.Simple
import Map.Internal

main :: IO ()
main = do
  output <- parseDhallFile "./hygiea/src/Planning/Examples/output.dhall"
  input <- parseDhallFile "./hygiea/src/Planning/Examples/input.dhall"
  -- see TODO in the dhall file. 
  inputList <- parseDhallFile "./hygiea/src/Planning/Examples/input_list.dhall"
  putStrLn "\nDhall shape from input.dhall for each line of Csv\n"
  print input

  putStrLn "\nRaw csv:\n"
  inputCsv <- toCsv True "./hygiea/src/Planning/Examples/input.csv"
  print inputCsv

  putStrLn "\nParse internal representation from Csv:\n"
  -- NOTE inputList
  let inputDecode = decodeMapSchemaAuto @TestAtomic inputList
  print $ tryParseRecords inputDecode inputCsv

    {- OLD -}
  --putStrLn "Dhall expected"
  --print (dhallExpected inputDecode)
  --putStrLn ""
  --putStrLn "Cohort builder is ProjEvent -> ProjCensoredOccurrence"
  --putStrLn "simply attaching 'after' reason and converting"
  --putStrLn "Test consumes the cohortBuilder and the internal input/output maps"
  --putStrLn "Is input == output when tested this way?"
  --let result = T.testIt @ProjInterval @ProjOccurrence inputDhall outputDhall
  --let result2 = T.testIt @ProjInterval @ProjOccurrence inputDhall outputDhall
  --print result
