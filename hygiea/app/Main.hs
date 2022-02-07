{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Planning.Test
import Planning.Decode

main :: IO ()
main = do
  output <- parseDhallFile "./hygiea/src/Planning/Examples/example.dhall"
  putStrLn "Parsed from Dhall:" 
  print output
