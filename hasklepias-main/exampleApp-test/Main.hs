{-| Tests of the example application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Exception             ( tryJust )
import           Control.Monad                 ( guard )
import qualified Data.ByteString.Lazy          as B
import           Data.Time                     ( getCurrentTime )
import           Data.Time.Clock.POSIX         ( utcTimeToPOSIXSeconds )
import           Data.Time.Clock               ( nominalDiffTimeToSeconds )
import           Data.Char                     ( isDigit )
import           Hasklepias
import           Hasklepias.ExampleApp
import           Hasklepias.MakeCohortApp       ( runApp )
import           System.IO.Error
import           System.Process
import           System.Environment
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Silver

-- Create a unique ID based on the GitLab environmental variable $CI_PIPELINE_ID
-- if one is defined, otherwise the computation fails with `isDoesNotExistError`
ciPipelineId :: IO String
ciPipelineId = getEnv "CI_PIPELINE_ID"

-- Use the value of `ciPipelineId` if it was able to be obtained, otherwise use
-- the number of seconds since the epoch as a fallback
sessionId :: IO String
sessionId = do
  r <- tryJust (guard . isDoesNotExistError) ciPipelineId
  case r of
    Left  e -> fmap (show . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) getCurrentTime
    Right v -> ciPipelineId

s3TestDataKey :: IO String
s3TestDataKey = pure "hasklepias/sandbox-testdata/testData-" <> sessionId <> pure ".jsonl"

-- Write the test data to S3. If the command returns a non-zero exit code then
-- an exception is raised
writeTestDataToS3 :: IO ()
writeTestDataToS3 = pure "aws s3 cp exampleApp-test/test/testData.jsonl s3://download.novisci.com/" <> s3TestDataKey >>= callCommand

appTestRw :: IO ()
appTestRw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
                          exampleAppRW
  B.writeFile "exampleApp-test/test/testrw.json" r

appStdinRw :: IO ()
appStdinRw =
  callCommand
    "< exampleApp-test/test/testData.jsonl exampleAppRW -o exampleApp-test/test/stdinrw.json"

appS3inRw :: IO ()
appS3inRw = do
  -- cmd <- pure "exampleAppRW -o exampleApp-test/test/s3inrw.json -r us-east-1 -b download.novisci.com -k hasklepias/sandbox-testdata/testData-" <> sessionId <> pure ".jsonl"
  cmd <- pure "exampleAppRW -o exampleApp-test/test/s3inrw.json -r us-east-1 -b download.novisci.com -k " <> s3TestDataKey
  callCommand
    cmd

appTestCw :: IO ()
appTestCw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
                          exampleAppCW
  B.writeFile "exampleApp-test/test/testcw.json" r

appStdinCw :: IO ()
appStdinCw =
  callCommand
    "< exampleApp-test/test/testData.jsonl exampleAppCW -o exampleApp-test/test/stdincw.json"

appTestEmptyRw :: IO ()
appTestEmptyRw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testEmptyData.jsonl")
                          exampleAppRW
  B.writeFile "exampleApp-test/test/testemptyrw.json" r

appStdinEmptyRw :: IO ()
appStdinEmptyRw =
  callCommand
    "< exampleApp-test/test/testEmptyData.jsonl exampleAppRW -o exampleApp-test/test/stdinemptyrw.json"

appTestEmptyCw :: IO ()
appTestEmptyCw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testEmptyData.jsonl")
                          exampleAppCW
  B.writeFile "exampleApp-test/test/testemptycw.json" r

appStdinEmptyCw :: IO ()
appStdinEmptyCw =
  callCommand
    "< exampleApp-test/test/testEmptyData.jsonl exampleAppCW -o exampleApp-test/test/stdinemptycw.json"

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
  , goldenVsFile "ExampleApp of row-wise cohort reading from S3"
                 "exampleApp-test/test/testrw.golden"
                 "exampleApp-test/test/s3inrw.json"
                 appS3inRw
  , goldenVsFile "ExampleApp of row-wise cohort with empty data from file"
                 "exampleApp-test/test/testemptyrw.golden"
                 "exampleApp-test/test/testemptyrw.json"
                 appTestEmptyRw
  , goldenVsFile
    "ExampleApp of row-wise cohort with empty data from standard input"
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
  , goldenVsFile
    "ExampleApp of column-wise cohort with empty data from standard input"
    "exampleApp-test/test/testemptycw.golden"
    "exampleApp-test/test/stdinemptycw.json"
    appStdinEmptyCw
  ]

main :: IO ()
main = do
  writeTestDataToS3
  defaultMain tests
  -- removeTestDataFromS3
