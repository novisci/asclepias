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
import           System.Directory               ( createDirectoryIfMissing )
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

-- Enumeration of the test applications
data AppType = AppRowWise | AppColumnWise
instance Show AppType where
  show AppRowWise = "row-wise"
  show AppColumnWise = "column-wise"

-- Enumeration of the test data cases
data TestDataType = TestDataEmpty | TestDataSmall
instance Show TestDataType where
  show TestDataEmpty = "empty data"
  show TestDataSmall = "small data"

-- Enumeration of input sources
data TestInputType = TestInputFile | TestInputStdin | TestInputS3
instance Show TestInputType where
  show TestInputFile = "file"
  show TestInputStdin = "standard input"
  show TestInputS3 = "S3"

-- Enumeration of input sources
data TestOutputType = TestOutputFile | TestOutputStdout | TestOutputS3
instance Show TestOutputType where
  show TestOutputFile = "file"
  show TestOutputStdout = "standard output"
  show TestOutputS3 = "S3"

-- Create the local filepath where the test data is stored
localInputDataLoc :: TestDataType -> String
localInputDataLoc TestDataEmpty = "exampleApp-test/test/testEmptyData.jsonl"
localInputDataLoc TestDataSmall = "exampleApp-test/test/testData.jsonl"

-- -- Create the local filepath where the cohort build is written to
-- localOutputDataLoc :: TestDataType -> String
-- localOutputDataLoc TestDataEmpty = "exampleApp-test/test/stdinemptyrw.json"
-- localOutputDataLoc TestDataSmall = "exampleApp-test/test/stdinrw.json"

-- Create the S3 key where the test data will be located (once paired with a bucket)
s3TestDataKey :: String -> TestDataType -> String
s3TestDataKey sessionId TestDataEmpty = "hasklepias/sandbox-testdata/testEmptyData-" ++ sessionId ++ "-empty.jsonl"
s3TestDataKey sessionId TestDataSmall = "hasklepias/sandbox-testdata/testData-" ++ sessionId ++ "-small.jsonl"

-- -- FIXME
-- -- Create the S3 key where the results will be located (once paired with a bucket)
-- s3TestDataKey :: String -> TestDataType -> String
-- s3TestDataKey sessionId TestDataEmpty = "hasklepias/sandbox-testdata/testEmptyData-" ++ sessionId ++ "-empty.jsonl"
-- s3TestDataKey sessionId TestDataSmall = "hasklepias/sandbox-testdata/testData-" ++ sessionId ++ "-small.jsonl"

-- Create the S3 URI where the test data will be located
s3TestDataURI  :: String -> String
s3TestDataURI key = "s3://download.novisci.com/" ++ key

-- Copy test data to S3
writeTestDataToS3 :: String -> TestDataType -> IO ()
writeTestDataToS3 sessionId testDataType = pure cmd >>= callCommand where
  from = localInputDataLoc testDataType
  to   = s3TestDataURI $ s3TestDataKey sessionId testDataType
  cmd  = "aws s3 cp " ++ from ++ " " ++ to

-- Delete test data from S3
removeTestDataFromS3 :: String -> TestDataType -> IO ()
removeTestDataFromS3 sessionId testDataType = pure cmd >>= callCommand where
  uri = s3TestDataURI $ s3TestDataKey sessionId testDataType
  cmd = "aws s3 rm " ++ uri

-- Copy test results from S3
copyResultsFroms3 :: AppType -> TestDataType -> TestInputType -> TestOutputType -> IO ()
copyResultsFroms3 appType testDataType testInputType testOutputType = pure cmd >>= callCommand where
  let filename = localOutputFilename appType testDataType testInputType testOutputType

  uri = s3TestDataURI $ s3TestDataKey sessionId testDataType
  cmd = "aws s3 rm " ++ uri


-- appTestRw :: IO ()
-- appTestRw = do
--   r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
--                           exampleAppRW
--   B.writeFile "exampleApp-test/test/testrw.json" r

-- appStdinRw :: IO ()
-- appStdinRw =
--   callCommand
--     "< exampleApp-test/test/testData.jsonl exampleAppRW -o exampleApp-test/test/stdinrw.appTest :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> IO ()
appTest sessionId appType testDataType testInputType testOutputType = do
  let appCmd = case appType of
        AppRowWise -> "exampleAppRW"
        AppColumnWise -> "exampleAppCW"
  let inputFragm = case testInputType of
        TestInputFile -> "-f " ++ localInputDataLoc testDataType
        TestInputStdin -> "< " ++ localInputDataLoc testDataType
        TestInputS3 -> "-r us-east-1 -b download.novisci.com -k " ++ s3TestDataKey sessionId testDataType
  let outputFragm = case testOutputType of
        TestOutputFile -> "-o " ++ localOutputFilepath (localOutputFilename appType testDataType testInputType testOutputType)
        TestOutputStdout -> "> " ++ localOutputFilepath (localOutputFilename appType testDataType testInputType testOutputType)
        TestOutputS3 -> "--outregion us-east-1 --outbucket download.novisci.com --outkey " ++ s3TestDataKey sessionId testDataType
  let cmd = appCmd ++ " " ++ inputFragm ++ " " ++ outputFragm
  print $ "TEST COMMAND:  " ++ cmd
  let isS3in = case testInputType of
        TestInputS3 -> True
        _           -> False
  let isS3out = case testOutputType of
        TestOutputS3 -> True
        _            -> False
  pure cmd >>= callCommand
  if isS3out then copyTestDataFromS3 sessionId testDataType else pure ()
  if isS3out then removeTestDataFromS3 sessionId testDataType else pure ()

-- appTestCw :: IO ()
-- appTestCw = do
--   r <- runAppWithLocation (Local "exampleApp-test/test/testData.jsonl")
--                           exampleAppCW
--   B.writeFile "exampleApp-test/test/testcw.json" r

-- appStdinCw :: IO ()
-- appStdinCw =
--   callCommand
--     "< exampleApp-test/test/testData.jsonl exampleAppCW -o exampleApp-test/test/stdincw.json"

-- appS3inCw :: String -> TestDataType -> IO ()
-- appS3inCw sessionId testDataType = do
--   let key = s3TestDataKey sessionId testDataType
--   let cmd = "exampleAppCW -o exampleApp-test/test/s3incw.json -r us-east-1 -b download.novisci.com -k " ++ key
--   pure cmd >>= callCommand

-- appTestEmptyRw :: IO ()
-- appTestEmptyRw = do
--   r <- runAppWithLocation (Local "exampleApp-test/test/testEmptyData.jsonl")
--                           exampleAppRW
--   B.writeFile "exampleApp-test/test/testemptyrw.json" r

-- appStdinEmptyRw :: IO ()
-- appStdinEmptyRw =
--   callCommand
--     "< exampleApp-test/test/testEmptyData.jsonl exampleAppRW -o exampleApp-test/test/stdinemptyrw.json"

-- appTestEmptyCw :: IO ()
-- appTestEmptyCw = do
--   r <- runAppWithLocation (Local "exampleApp-test/test/testEmptyData.jsonl")
--                           exampleAppCW
--   B.writeFile "exampleApp-test/test/testemptycw.json" r

-- appStdinEmptyCw :: IO ()
-- appStdinEmptyCw =
--   callCommand
--     "< exampleApp-test/test/testEmptyData.jsonl exampleAppCW -o exampleApp-test/test/stdinemptycw.json"

constructTestName :: AppType -> TestDataType -> TestInputType -> TestOutputType -> String
constructTestName appType testDataType testInputType testOutputType = concat
  [ "ExampleApp of a "
  , show appType
  , " cohort performed on "
  , show testDataType
  , " reading from "
  , show testInputType
  , " and writing to "
  , show testOutputType
  ]

constructTestLocGolden :: AppType -> TestDataType -> String
constructTestLocGolden appType testDataType = concat
  [ "exampleApp-test/test/test"
  , case testDataType of
      TestDataEmpty -> "empty"
      TestDataSmall -> ""
  , case appType of
      AppRowWise -> "rw"
      AppColumnWise -> "cw"
  , ".golden"
  ]

localOutputFilepath :: String -> String
localOutputFilepath = ("exampleApp-test/results/" ++)

localOutputFilename :: AppType -> TestDataType -> TestInputType -> TestOutputType -> String
localOutputFilename appType testDataType testInputType testOutputType = concat
  [ "results-"
  , case appType of
      AppRowWise -> "rw"
      AppColumnWise -> "cw"
  , "-"
  , case testDataType of
      TestDataEmpty -> "emptydata"
      TestDataSmall -> "small"
  , "-"
  , case testInputType of
      TestInputFile -> "filein"
      TestInputStdin -> "stdin"
      TestInputS3 -> "s3in"
  , "-"
  , case testOutputType of
      TestOutputFile -> "fileout"
      TestOutputStdout -> "stdout"
      TestOutputS3 -> "s3out"
  , ".json"
  ]

appGoldenVsFile :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> TestTree
appGoldenVsFile sessionId appType testDataType testInputType testOutputType =
  goldenVsFile
    (constructTestName appType testDataType testInputType testOutputType)
    (constructTestLocGolden appType testDataType)
    (localOutputFilepath (localOutputFilename appType testDataType testInputType testOutputType))
    (appTest sessionId appType testDataType testInputType testOutputType)

tests :: String -> TestTree
tests sessionId = testGroup
  "Tests of exampleApp"
  [ appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputFile  TestOutputStdout
  -- , appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputFile  TestOutputS3
  -- , appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputStdin TestOutputFile
  -- , appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputStdin TestOutputStdout
  -- , appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputStdin TestOutputS3
  -- , appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputS3    TestOutputFile
  -- , appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputS3    TestOutputStdout
  -- , appGoldenVsFile sessionId AppRowWise    TestDataEmpty TestInputS3    TestOutputS3
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputFile  TestOutputFile
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputFile  TestOutputStdout
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputFile  TestOutputS3
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputStdin TestOutputFile
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputStdin TestOutputStdout
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputStdin TestOutputS3
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputS3    TestOutputFile
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputS3    TestOutputStdout
  -- , appGoldenVsFile sessionId AppRowWise    TestDataSmall TestInputS3    TestOutputS3
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputFile  TestOutputFile
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputFile  TestOutputStdout
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputFile  TestOutputS3
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputStdin TestOutputFile
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputStdin TestOutputStdout
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputStdin TestOutputS3
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputS3    TestOutputFile
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputS3    TestOutputStdout
  -- , appGoldenVsFile sessionId AppColumnWise TestDataEmpty TestInputS3    TestOutputS3
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputFile  TestOutputFile
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputFile  TestOutputStdout
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputFile  TestOutputS3
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputStdin TestOutputFile
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputStdin TestOutputStdout
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputStdin TestOutputS3
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputS3    TestOutputFile
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputS3    TestOutputStdout
  -- , appGoldenVsFile sessionId AppColumnWise TestDataSmall TestInputS3    TestOutputS3
  ]

main :: IO ()
main = do
  createDirectoryIfMissing True "exampleApp-test/results"
  sessionId <- getSessionId
  -- writeTestDataToS3 sessionId TestDataSmall
  defaultMain (tests sessionId)
  -- removeTestDataFromS3 sessionId TestDataSmall
