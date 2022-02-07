{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Planning.Test as T
import Planning.Output
import Planning.Decode
import Planning.Examples.Simple

main :: IO ()
main = do
  output <- parseDhallFile "./hygiea/src/Planning/Examples/example.dhall"
  putStrLn "Parsed from Dhall:"
  print output
  -- TODO: rework Testable a bit
  let outcomes = T.toOutput inputs :: [ProjectOutcome]
  let result = from outcomes :: OutputData
  putStrLn "Cohort produced:"
  print result
  putStrLn "Result of test:"
  print (result == output)

