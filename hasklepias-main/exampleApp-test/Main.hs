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
import           System.Process

appTestRw :: IO ()
appTestRw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
                          exampleAppRW
  B.writeFile "exampleApp-test/test/testrw.json" r

appStdinRw :: IO ()
appStdinRw = callCommand "< exampleApp-test/test/testData.jsonl exampleAppRW -o exampleApp-test/test/stdinrw.json"

appTestCw :: IO ()
appTestCw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
                          exampleAppCW
  B.writeFile "exampleApp-test/test/testcw.json" r

appStdinCw :: IO ()
appStdinCw = callCommand "< exampleApp-test/test/testData.jsonl exampleAppCW -o exampleApp-test/test/stdincw.json"

appTestEmptyRw :: IO ()
appTestEmptyRw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testEmptyData.jsonl")
                          exampleAppRW
  B.writeFile "exampleApp-test/test/testemptyrw.json" r

appStdinEmptyRw :: IO ()
appStdinEmptyRw = callCommand "< exampleApp-test/test/testEmptyData.jsonl exampleAppRW -o exampleApp-test/test/stdinemptyrw.json"

appTestEmptyCw :: IO ()
appTestEmptyCw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testEmptyData.jsonl")
                          exampleAppCW
  B.writeFile "exampleApp-test/test/testemptycw.json" r

appStdinEmptyCw :: IO ()
appStdinEmptyCw = callCommand "< exampleApp-test/test/testEmptyData.jsonl exampleAppCW -o exampleApp-test/test/stdinemptycw.json"

tests :: TestTree
tests = testGroup
  "Tests of exampleApp"
  [ goldenVsFile "ExampleApp of row-wise cohort reading from file"
                 "exampleApp-test/test/testrw.golden"
                 "exampleApp-test/test/testrw.json"
                 appTestRw
  , goldenVsFile "ExampleApp of row-wise cohort reading from standard input"
                 "exampleApp-test/test/testrw.golden"
                 "exampleApp-test/test/stdinrw.json"
                 appStdinRw
  , goldenVsFile "ExampleApp of row-wise cohort with empty data from file"
                 "exampleApp-test/test/testemptyrw.golden"
                 "exampleApp-test/test/testemptyrw.json"
                 appTestEmptyRw
  , goldenVsFile "ExampleApp of row-wise cohort with empty data from standard input"
                 "exampleApp-test/test/testemptyrw.golden"
                 "exampleApp-test/test/stdinemptyrw.json"
                 appStdinEmptyRw
  , goldenVsFile "ExampleApp of column-wise cohort from file"
                 "exampleApp-test/test/testcw.golden"
                 "exampleApp-test/test/testcw.json"
                 appTestCw
  , goldenVsFile "ExampleApp of column-wise cohort from standard input"
                 "exampleApp-test/test/testcw.golden"
                 "exampleApp-test/test/stdincw.json"
                 appStdinCw
  , goldenVsFile "ExampleApp of column-wise cohort with empty data from file"
                 "exampleApp-test/test/testemptycw.golden"
                 "exampleApp-test/test/testemptycw.json"
                 appTestEmptyCw
  , goldenVsFile "ExampleApp of column-wise cohort with empty data from standard input"
                 "exampleApp-test/test/testemptycw.golden"
                 "exampleApp-test/test/stdinemptycw.json"
                 appStdinEmptyCw
  ]

main :: IO ()
main = defaultMain tests
