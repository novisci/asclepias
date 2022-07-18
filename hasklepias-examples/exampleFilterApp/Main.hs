{-|
Module      : ExampleFilterApp
Description : Demostrates how to filter event lines
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

To run as an example: 
cat hasklepias-main/exampleApp/exampleData.jsonl | cabal exec exampleApp
-}

{-# LANGUAGE NoImplicitPrelude #-}
module Main
  ( main
  ) where
import           AppExamples.FilterApp
import           Hasklepias

main :: IO ()
main = exampleFilterApp
