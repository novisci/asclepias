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
getCIPipelineId :: IO String
getCIPipelineId = getEnv "CI_PIPELINE_ID"

-- Use the value of `getCIPipelineId` if it was able to be obtained, otherwise use
-- the number of seconds since the epoch as a fallback
getSessionId :: IO String
getSessionId = do
  r <- tryJust (guard . isDoesNotExistError) getCIPipelineId
  case r of
    Left  e -> fmap (show . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) getCurrentTime
    Right v -> getCIPipelineId

s3TestDataKey :: String -> String
s3TestDataKey sessionId = "hasklepias/sandbox-testdata/testData-" ++ sessionId ++ ".jsonl"

s3TestDataURI  :: String -> String
s3TestDataURI sessionId = "s3://download.novisci.com/" ++ s3TestDataKey sessionId

writeTestDataToS3 :: String -> IO ()
writeTestDataToS3 uri = pure ("aws s3 cp exampleApp-test/test/testData.jsonl " ++ uri) >>= callCommand

removeTestDataFromS3 :: String -> IO ()
removeTestDataFromS3 uri = pure ("aws s3 rm " ++ uri) >>= callCommand

appTestRw :: IO ()
appTestRw = do
  r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
                          exampleAppRW
  B.writeFile "exampleApp-test/test/testrw.json" r

appStdinRw :: IO ()
appStdinRw =
  callCommand
    "< exampleApp-test/test/testData.jsonl exampleAppRW -o exampleApp-test/test/stdinrw.json"

appS3inRw :: String -> IO ()
appS3inRw sessionKey = do
  let cmd = pure ("exampleAppRW -o exampleApp-test/test/s3inrw.json -r us-east-1 -b download.novisci.com -k " ++ sessionKey)
  cmd >>= callCommand

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

tests :: String -> TestTree
tests sessionId = testGroup
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
                 (appS3inRw sessionId)
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
  sessionId <- getSessionId
  let sessionKey = s3TestDataKey sessionId
  let sessionURI = s3TestDataURI sessionId
  writeTestDataToS3 sessionURI
  defaultMain (tests sessionKey)
  removeTestDataFromS3 sessionURI
