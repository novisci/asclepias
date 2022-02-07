{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Planning.Test as T
import Planning.Decode
import Planning.Examples.Simple

main :: IO ()
main = do
  output <- parseDhallFile "./hygiea/src/Planning/Examples/example.dhall"
  putStrLn "Parsed from Dhall:" 
  print output
  -- TODO: rework Testable a bit
  --let result = T.testIt inputs output
  --print result

