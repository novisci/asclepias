{-| Tests of the example application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy          as B
import           Hasklepias
import           Hasklepias.ExampleApp
import           Hasklepias.MakeCohortApp       ( runApp )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Silver

appTestRw :: IO ()
appTestRw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
                          exampleAppRW
  B.writeFile "exampleApp-test/test/testrw.json" r

appTestCw :: IO ()
appTestCw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
                          exampleAppCW
  B.writeFile "exampleApp-test/test/testcw.json" r

appTestEmptyRw :: IO ()
appTestEmptyRw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testEmptyData.jsonl")
                          exampleAppRW
  B.writeFile "exampleApp-test/test/testemptyrw.json" r

appTestEmptyCw :: IO ()
appTestEmptyCw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testEmptyData.jsonl")
                          exampleAppCW
  B.writeFile "exampleApp-test/test/testemptycw.json" r

tests :: TestTree
tests = testGroup
  "Tests of exampleApp"
  [ goldenVsFile "ExampleApp of row-wise cohort"
                 "exampleApp-test/test/testrw.golden"
                 "exampleApp-test/test/testrw.json"
                 appTestRw
  , goldenVsFile "ExampleApp of row-wise cohort with empty data"
                 "exampleApp-test/test/testemptyrw.golden"
                 "exampleApp-test/test/testemptyrw.json"
                 appTestEmptyRw
  , goldenVsFile "ExampleApp of column-wise cohort"
                 "exampleApp-test/test/testcw.golden"
                 "exampleApp-test/test/testcw.json"
                 appTestCw
  , goldenVsFile "ExampleApp of column-wise cohort with empty data"
                 "exampleApp-test/test/testemptycw.golden"
                 "exampleApp-test/test/testemptycw.json"
                 appTestEmptyCw
  ]

main :: IO ()
main = defaultMain tests
