{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Hygiea.Test as Test
--import Examples.Simple
import Hygiea.Map
import Hygiea.Internal.Dhall

main :: IO ()
main = do
  output <- parseDhallFile "./hygiea/src/Examples/output.dhall"
  input <- parseDhallFile "./hygiea/src/Examples/input.dhall"
  -- see TODO in the dhall file. 
  inputList <- parseDhallFile "./hygiea/src/Examples/input_list.dhall"
  putStrLn "\nDhall shape from input.dhall for each line of Csv\n"
  print input

  putStrLn "\nRaw csv:\n"
  inputCsv <- toCsv True "./hygiea/src/Examples/input.csv"
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
  --let result = Test.testIt @ProjInterval @ProjOccurrence inputDhall outputDhall
  --let result2 = Test.testIt @ProjInterval @ProjOccurrence inputDhall outputDhall
  --print result
