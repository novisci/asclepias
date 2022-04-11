{-|
Module      : ExampleCohortApp
Description : Demostrates how to define a cohort using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

To run as an example: 
cat hasklepias-main/exampleData/exampleData.jsonl | cabal exec exampleApp
-}

{-# LANGUAGE NoImplicitPrelude #-}
module Main
  ( main
  ) where
import           Hasklepias
import           AppExamples.CohortApp

main :: IO ()
main = runApp exampleAppRW
