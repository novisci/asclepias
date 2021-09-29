{-| Tests of the example application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main 
  ( main
  ) where

import Hasklepias
import Hasklepias.ExampleApp
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Silver
import qualified Data.ByteString.Lazy          as B
import Hasklepias.MakeApp (runApp)

appTestRw :: IO ()
appTestRw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl") exampleAppRW 
  B.writeFile "exampleApp-test/test/testrw.json" r

tests :: TestTree
tests = testGroup
  "Tests of exampleApp"
  [  goldenVsFile "ExampleApp of row-wise cohort"
                 "exampleApp-test/test/testrw.golden"
                 "exampleApp-test/test/testrw.json"
                 appTestRw
--   , goldenVsFile "Collection of column-wise cohorts"
--                  "test/tests/testcw.golden"
--                  "test/tests/testcw.json"
--                  appTestCw
  ]

main :: IO ()
main = defaultMain tests