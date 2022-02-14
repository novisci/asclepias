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
  --putStrLn "Parsed input shape from Dhall:"
  --print input
  putStrLn ""
  putStrLn "Input text:"
  print testInputText
  putStrLn ""
  putStrLn "Converted to internal map type:"
  inputDhall <- mapInputSchema input testInputText :: IO TestMap
  print inputDhall
  putStrLn ""
  putStrLn "Output parsed in same way:"
  outputDhall <- mapInputSchema output testOutputText :: IO TestMap
  print outputDhall
  putStrLn ""
  putStrLn "Cohort builder is ProjEvent -> ProjCensoredOccurrence"
  putStrLn "simply attaching 'after' reason and converting"
  putStrLn "Test consumes the cohortBuilder and the internal input/output maps"
  putStrLn "Is input == output when tested this way?"
  let result = T.testIt @ProjInterval @ProjOccurrence inputDhall outputDhall
  print result
