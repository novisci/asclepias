{-| Tests of the example application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad                  ( when )  -- FIXME after removing unneeded code using this
import           Control.Exception             ( catch
                                               , throwIO )
import qualified Data.ByteString.Lazy          as B
import           Data.Char                     ( isDigit )
import           Hasklepias
import           TestUtils.BuildLargeTestData
import           Hasklepias.ExampleApp
import           TestUtils.ConstructTestTree
import           TestUtils.SessionId
import           TestUtils.TestCases
import           TestUtils.S3Utils                        -- FIXME after removing unneeded code using this
import           Hasklepias.MakeCohortApp       ( runApp )
import           System.Directory               ( createDirectoryIfMissing
                                                , removeDirectoryRecursive
                                                , removePathForcibly
                                                )
import           System.Exit                    ( ExitCode )
import           System.Process                 ( callCommand )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Silver

localTestDataDir :: String
localTestDataDir = "exampleApp-test/test/"

localResultsDir :: String
localResultsDir = "exampleApp-test/results/"

s3Bucket :: String
s3Bucket = "download.novisci.com"

s3RootDir :: String
s3RootDir = "hasklepias/sandbox-testapps/cohortApp/"

-- Run the tests
--
-- Note that changing the number of events for the "many events" test data
-- doesn't change the output of the cohort creation since the cohort definition
-- throws that data away anyway, so for this reason we create a golden file and
-- store it in the repository. On the other hand, for the "many subjects" test
-- data we generate the data and corresponding golden file each time we do the
-- testing so as to (i) prevent storing large files in the repository, and (ii)
-- so that we can change the number of test subjects with ease.
main :: IO ()
main = do

  -- Generate a unique session ID and ensure that the results directory exists
  sessionId <- getSessionId
  createDirectoryIfMissing True localResultsDir

  -- Generate the many subjects test data, the many events test data, and the
  -- many subjects row-wise and column-wise golden files. Note that the "many
  -- event" golden files are included as part of the repository
  generateTestDataManySubjectsPtl
  generateTestDataManyEventPtl
  generateGoldenManySubjectsRwPtl
  generateGoldenManySubjectsCwPtl

  -- Copy the test data to S3 with session-specific keys to avoid collisions
  writeTestDataToS3 sessionId TestDataEmpty
  writeTestDataToS3 sessionId TestDataSmall
  writeTestDataToS3 sessionId TestDataManySubj
  writeTestDataToS3 sessionId TestDataManyEvent

  -- Run the tests and perform cleanup. Note that ANY CODE WRITTEN AFTER THIS
  -- EXPRESION WILL BE SILENTLY IGNORED
  defaultMain (createTestsCartesian "Cohort creation tests" (appGoldenVsFile' sessionId))
    `catch` (\e -> do
      -- removeDirectoryRecursive localResultsDir
      -- s3RecursiveRm s3RootDir sessionId  -- TODO: uncomment!
      -- s3RecursiveRm s3RootDir sessionId  -- TODO: uncomment!
      -- FIXME: add generated test files and golden files to gitignore?
      throwIO (e :: ExitCode))

appGoldenVsFile' :: String -> TestScenarioCohort -> TestTree
appGoldenVsFile' sessionId =
  appGoldenVsFile
    constructTestName'
    createFilepathForTest'
    createFilepathForGolden'
    createFilepathForResult'
    (appTest' sessionId)


appTest' :: String -> TestScenarioCohort -> IO ()
appTest' sessionId =
  appTest
    (\_ -> pure ())
    (appTestCmd' sessionId)
    (postCmdHookS3 (createS3UriForResult sessionId) createFilepathForResult')

appTestCmd' :: String -> TestScenarioCohort -> IO ()
appTestCmd' sessionId = appTestCmd (appTestCmdString' sessionId)

appTestCmdString' :: String -> TestScenarioCohort -> String
appTestCmdString' sessionId testScenario =
  appTestCmdString
    constructTestExecutableFragm
    (constructTestInputFragm' sessionId)
    (constructTestOutputFragm' sessionId)
    testScenario

constructTestExecutableFragm :: TestScenarioCohort -> String
constructTestExecutableFragm testScenarioCohort =
   case getCohortAppType testScenarioCohort of
    AppRowWise -> "exampleAppRW"
    AppColumnWise -> "exampleAppCW"

constructTestInputFragm' :: String -> TestScenarioCohort -> String
constructTestInputFragm' sessionId testScenarioCohort =
  constructTestInputFragmFSS
    createFilepathForTest'
    (const s3Bucket)
    (createS3KeyForTest sessionId)
    testScenarioCohort

constructTestOutputFragm' :: String -> TestScenarioCohort -> String
constructTestOutputFragm' sessionId =
  constructTestOutputFragmFSS
    createFilepathForResult'
    (const s3Bucket)
    (createS3KeyForResult sessionId)

-- -- Conduct a single test
-- appGoldenVsFile :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> TestTree
-- appGoldenVsFile sessionId appType testDataType testInputType testOutputType =
--   goldenVsFile
--     (constructTestName appType testDataType testInputType testOutputType)
--     (createFilepathForGolden appType testDataType)
--     (createFilepathForResult appType testDataType testInputType testOutputType)
--     (appTestLocal sessionId appType testDataType testInputType testOutputType)

-- Build a shell command represented by string and run the command as a
-- subprocess, where the command is a cohort-building application. If the
-- application writes the results to S3 then copy those results back to the
-- local filesystem
appTestLocal :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> IO ()
appTestLocal sessionId appType testDataType testInputType testOutputType = do
  let outFilename = createFilenameForResult appType testDataType testInputType testOutputType
  let isS3out = case testOutputType of
        TestOutputS3 -> True
        _ -> False
  let cmd = appTestCmdLocal sessionId appType testDataType testInputType testOutputType
  print $ "TEST COMMAND:  " ++ cmd
  pure cmd >>= callCommand
  when isS3out $
    s3Copy
      (convNameToS3UriResult sessionId outFilename)
      (convNameToPathResult outFilename)

-- Construct a string representing a shell command that runs one of the testing
-- cohort-building applications on the test data
--
-- Note that if the input is specified as coming from standard input, then a
-- fragment like `"< /path/to/file"` is inserted in the middle of the command
-- string. While this is not usual practice, the shell removes the fragment
-- prior to processing and things do indeed work as intended
appTestCmdLocal :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> String
appTestCmdLocal sessionId appType testDataType testInputType testOutputType =
  appCmd ++ " " ++ inputFragm ++ " " ++ outputFragm
  where
    inFilename = createFilenameForTest testDataType
    outFilename = createFilenameForResult appType testDataType testInputType testOutputType
    appCmd = case appType of
        AppRowWise -> "exampleAppRW"
        AppColumnWise -> "exampleAppCW"
    inputFragm = case testInputType of
        TestInputFile -> "-f " ++ convNameToPathTest inFilename
        TestInputStdin -> "< " ++ convNameToPathTest inFilename
        TestInputS3 -> "-r us-east-1 -b " ++ s3Bucket ++ " -k " ++ convNameToS3KeyTest sessionId inFilename
    outputFragm = case testOutputType of
        TestOutputFile -> "-o " ++ convNameToPathResult outFilename
        TestOutputStdout -> "> " ++ convNameToPathResult outFilename
        TestOutputS3 -> "--outregion us-east-1 --outbucket " ++ s3Bucket ++ " --outkey " ++ convNameToS3KeyResult sessionId outFilename

-- Construct the test data for the "many subjects" test scenario and write it to
-- the filesystem
generateTestDataManySubjectsPtl :: IO ()
generateTestDataManySubjectsPtl =
  generateTestDataManySubjects
    (localTestDataDir ++ "testData.jsonl")
    (localTestDataDir ++ "testManySubjects.jsonl")

-- Construct the test data for the "many events" test scenario and write it to
-- the filesystem
generateTestDataManyEventPtl :: IO ()
generateTestDataManyEventPtl =
  generateTestDataManyEvents
    (localTestDataDir ++ "testData.jsonl")
    (localTestDataDir ++ "testManyEvents.jsonl")

-- Construct the golden file for the row-wise representation of the "many
-- subjects" test scenario and write it to the filesystem
generateGoldenManySubjectsRwPtl :: IO ()
generateGoldenManySubjectsRwPtl =
  generateGoldenManySubjectsRw
    (localTestDataDir ++ "testmanysubjectsrw.golden")

-- Construct the golden file for the column-wise representation of the "many
-- subjects" test scenario and write it to the filesystem
generateGoldenManySubjectsCwPtl :: IO ()
generateGoldenManySubjectsCwPtl =
  generateGoldenManySubjectsCw
    (localTestDataDir ++ "testmanysubjectscw.golden")

-- Create the local filepath where the test data is stored
createFilenameForTest :: TestDataType -> String
createFilenameForTest TestDataEmpty = "testEmptyData.jsonl"
createFilenameForTest TestDataSmall = "testData.jsonl"
createFilenameForTest TestDataManySubj = "testmanysubjects.jsonl"
createFilenameForTest TestDataManyEvent = "testmanyevents.jsonl"

-- Construct the filename for the output for a given test
createFilenameForResult' :: TestScenarioCohort -> String
createFilenameForResult' testScenarioCohort = concat
  [ "results-"
  , case getCohortAppType testScenarioCohort of
      AppRowWise -> "rw"
      AppColumnWise -> "cw"
  , "-"
  , case getCohortTestDataType testScenarioCohort of
      TestDataEmpty -> "emptydata"
      TestDataSmall -> "small"
      TestDataManySubj -> "manysubjectss"
      TestDataManyEvent -> "manyevents"
  , "-"
  , case getCohortTestInputType testScenarioCohort of
      TestInputFile -> "filein"
      TestInputStdin -> "stdin"
      TestInputS3 -> "s3in"
  , "-"
  , case getCohortTestOutputType testScenarioCohort of
      TestOutputFile -> "fileout"
      TestOutputStdout -> "stdout"
      TestOutputS3 -> "s3out"
  , ".json"
  ]

-- Construct the filename for the output for a given test
createFilenameForResult :: AppType -> TestDataType -> TestInputType -> TestOutputType -> String
createFilenameForResult appType testDataType testInputType testOutputType = concat
  [ "results-"
  , case appType of
      AppRowWise -> "rw"
      AppColumnWise -> "cw"
  , "-"
  , case testDataType of
      TestDataEmpty -> "emptydata"
      TestDataSmall -> "small"
      TestDataManySubj -> "manysubjectss"
      TestDataManyEvent -> "manyevents"
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

-- Construct the local filepath where the golden file is found for a given test
createFilenameForGolden' :: TestScenarioCohort -> String
createFilenameForGolden' testScenarioCohort =
  "test"
    ++ case getCohortTestDataType testScenarioCohort of
        TestDataEmpty -> "empty"
        TestDataSmall -> ""
        TestDataManySubj -> "manysubjects"
        TestDataManyEvent -> "manyevents"
    ++ case getCohortAppType testScenarioCohort of
        AppRowWise -> "rw"
        AppColumnWise -> "cw"
    ++ ".golden"

-- Construct the local filepath where the golden file is found for a given test
createFilenameForGolden :: AppType -> TestDataType -> String
createFilenameForGolden appType testDataType = concat
  [ "test"
  , case testDataType of
      TestDataEmpty -> "empty"
      TestDataSmall -> ""
      TestDataManySubj -> "manysubjects"
      TestDataManyEvent -> "manyevents"
  , case appType of
      AppRowWise -> "rw"
      AppColumnWise -> "cw"
  , ".golden"
  ]

createFilepathForTest' ::  TestScenarioCohort -> String
createFilepathForTest' testScenarioCohort =
  localTestDataDir
    ++ createFilenameForTest (getCohortTestDataType testScenarioCohort)

-- Helper function to create the local filpath from a filename
createFilepathForResult' :: TestScenarioCohort -> String
createFilepathForResult' testScenarioCohort =
  localResultsDir ++ createFilenameForResult' testScenarioCohort

-- -- Helper function to create the local filpath from a filename
-- createFilepathForResult' :: TestScenarioCohort -> String
-- createFilepathForResult' testScenarioCohort =
--   localResultsDir ++ createFilenameForResult' testScenarioCohort

-- Helper function to create the local filpath from a filename
createFilepathForResult :: AppType -> TestDataType -> TestInputType -> TestOutputType -> String
createFilepathForResult appType testDataType testInputType testOutputType =
  localResultsDir ++ createFilenameForResult appType testDataType testInputType testOutputType

createFilepathForGolden' :: TestScenarioCohort -> String
createFilepathForGolden' testScenarioCohort =
  localTestDataDir ++ createFilenameForGolden' testScenarioCohort

createFilepathForGolden :: AppType -> TestDataType -> String
createFilepathForGolden appType testDataType =
  localTestDataDir ++ createFilenameForGolden appType testDataType

convNameToPathTest :: String -> String
convNameToPathTest = (localTestDataDir ++)

convNameToPathResult :: String -> String
convNameToPathResult = (localResultsDir ++)

createS3KeyForTest :: String -> TestScenarioCohort -> String
createS3KeyForTest sessionId testScenarioCohort =
  convFilenameToS3KeyTest'
    sessionId
    (createFilenameForTest (getCohortTestDataType testScenarioCohort))

createS3KeyForResult :: String -> TestScenarioCohort -> String
createS3KeyForResult sessionId testScenarioCohort =
  convFilenameToS3KeyResult sessionId (createFilenameForResult' testScenarioCohort)

createS3UriForResult :: String -> TestScenarioCohort -> String
createS3UriForResult sessionId testScenarioCohort =
  convNameToS3UriResult sessionId (createFilenameForResult' testScenarioCohort)

-- Create the S3 key where the test data will be located (once paired with a
-- bucket)
convFilenameToS3KeyTest' :: String -> String -> String
convFilenameToS3KeyTest' filename sessionId =
  s3RootDir ++ sessionId ++ "/testdata/" ++ filename

-- Create the S3 key where the test data will be located (once paired with a
-- bucket)
convFilenameToS3KeyTest :: String -> String -> String
convFilenameToS3KeyTest filename sessionId =
  s3RootDir ++ sessionId ++ "/testdata/" ++ filename

convNameToS3KeyTest :: String -> String -> String
convNameToS3KeyTest sessionId filename =
  s3RootDir ++ sessionId ++ "/testdata/" ++ filename

-- Create the S3 key where the results will be located (once paired with a
-- bucket)
convFilenameToS3KeyResult :: String -> String -> String
convFilenameToS3KeyResult sessionId filename =
  s3RootDir ++ sessionId ++ "/results/" ++ filename

-- Create the S3 key where the results will be located (once paired with a
-- bucket)
convNameToS3KeyResult :: String -> String -> String
convNameToS3KeyResult sessionId filename =
  s3RootDir ++ sessionId ++ "/results/" ++ filename

-- Create the S3 key where the test data will be located (once paired with a
-- bucket)
convNameToS3UriTest :: String -> String -> String
convNameToS3UriTest sessionId filename = convS3KeyToUri $ convNameToS3KeyTest sessionId filename

-- Create the S3 key where the test data will be located (once paired with a
-- bucket)
convNameToS3UriResult :: String -> String -> String
convNameToS3UriResult sessionId filename = convS3KeyToUri $ convNameToS3KeyResult sessionId filename

constructTestName' :: TestScenarioCohort -> String
constructTestName' testScenarioCohort =
  "ExampleApp of a "
    ++ case getCohortAppType testScenarioCohort of
         AppRowWise-> "row-wise"
         AppColumnWise -> "column-wise"
    ++ " getCohortCohort performed on "
    ++ case getCohortTestDataType testScenarioCohort of
         TestDataEmpty -> "empty data"
         TestDataSmall -> "small data"
         TestDataManySubj -> "many subjects data"
         TestDataManyEvent -> "many evetns data"
    ++ " reading from "
    ++ case getCohortTestInputType testScenarioCohort of
         TestInputFile -> "file"
         TestInputStdin -> "standard input"
         TestInputS3 -> "S3"
    ++ " and writing to "
    ++ case getCohortTestOutputType testScenarioCohort of
         TestOutputFile -> "file"
         TestOutputStdout -> "standard output"
         TestOutputS3 -> "S3"

-- Construct a name to use as a label for a given test
constructTestName :: AppType -> TestDataType -> TestInputType -> TestOutputType -> String
constructTestName appType testDataType testInputType testOutputType = concat
  [ "ExampleApp of a "
  , case appType of
      AppRowWise-> "row-wise"
      AppColumnWise -> "column-wise"
  , " cohort performed on "
  , case testDataType of
      TestDataEmpty -> "empty data"
      TestDataSmall -> "small data"
      TestDataManySubj -> "many subjects data"
      TestDataManyEvent -> "many evetns data"
  , " reading from "
  , case testInputType of
      TestInputFile -> "file"
      TestInputStdin -> "standard input"
      TestInputS3 -> "S3"
  , " and writing to "
  , case testOutputType of
      TestOutputFile -> "file"
      TestOutputStdout -> "standard output"
      TestOutputS3 -> "S3"
  ]

-- Copy local test data to S3
writeTestDataToS3 :: String -> TestDataType -> IO ()
writeTestDataToS3 sessionId testDataType =
  s3Copy from to
  where
    filename = createFilenameForTest testDataType
    from = convNameToPathTest filename
    to = convNameToS3UriTest sessionId filename

convS3KeyToUri :: String -> String
convS3KeyToUri s3Key = "s3://" ++ s3Bucket ++ "/" ++ s3Key
