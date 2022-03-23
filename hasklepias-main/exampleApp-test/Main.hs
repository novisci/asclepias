{-| Tests of the example application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Exception             ( catch
                                               , throwIO )
import           Control.Monad                 ( when )
import qualified Data.ByteString.Lazy          as B
import           Data.Char                     ( isDigit )
import           Hasklepias
import           TestUtils.BuildLargeTestData
import           Hasklepias.ExampleApp
import           TestUtils.SessionId
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

s3TestDataDir :: String
s3TestDataDir = "hasklepias/sandbox-testapps/testdata/"

s3ResultsDir :: String
s3ResultsDir = "hasklepias/sandbox-testapps/results/"

-- Enumeration of the test applications
data AppType = AppRowWise | AppColumnWise

-- Enumeration of the test data cases
data TestDataType = TestDataEmpty | TestDataSmall | TestDataManySubj | TestDataManyEvent

-- Enumeration of input sources
data TestInputType = TestInputFile | TestInputStdin | TestInputS3

-- Enumeration of output sources
data TestOutputType = TestOutputFile | TestOutputStdout | TestOutputS3

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
  defaultMain (tests sessionId)
    `catch` (\e -> do
      -- removeDirectoryRecursive localResultsDir
      -- removeSessionDirFromS3 s3TestDataDir sessionId  -- TODO: uncomment!
      -- removeSessionDirFromS3 s3ResultsDir sessionId  -- TODO: uncomment!
      -- FIXME: add generated test files and golden files to gitignore?
      throwIO (e :: ExitCode))

-- Enumerate the test cases
tests :: String -> TestTree
tests sessionId = testGroup
  "Tests of exampleApp"
  [ appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputFile  TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputFile  TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputStdin TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputStdin TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputStdin TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputS3    TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputS3    TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataEmpty     TestInputS3    TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputFile  TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputFile  TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputStdin TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputStdin TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputStdin TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputS3    TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputS3    TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataSmall     TestInputS3    TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputFile  TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputFile  TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputStdin TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputStdin TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputStdin TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputS3    TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputS3    TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataManySubj  TestInputS3    TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputFile  TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputFile  TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputStdin TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputStdin TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputStdin TestOutputS3
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputS3    TestOutputFile
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputS3    TestOutputStdout
  , appGoldenVsFile sessionId AppRowWise    TestDataManyEvent TestInputS3    TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputFile  TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputFile  TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputStdin TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputStdin TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputStdin TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputS3    TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputS3    TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataEmpty     TestInputS3    TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputFile  TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputFile  TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputStdin TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputStdin TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputStdin TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputS3    TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputS3    TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataSmall     TestInputS3    TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputFile  TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputFile  TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputStdin TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputStdin TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputStdin TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputS3    TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputS3    TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataManySubj  TestInputS3    TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputFile  TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputFile  TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputFile  TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputStdin TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputStdin TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputStdin TestOutputS3
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputS3    TestOutputFile
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputS3    TestOutputStdout
  , appGoldenVsFile sessionId AppColumnWise TestDataManyEvent TestInputS3    TestOutputS3
  ]

-- Conduct a single test
appGoldenVsFile :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> TestTree
appGoldenVsFile sessionId appType testDataType testInputType testOutputType =
  goldenVsFile
    (constructTestName appType testDataType testInputType testOutputType)
    (createLocalFilepathForGolden appType testDataType)
    (createLocalFilepathForResults (createFilenameForResults appType testDataType testInputType testOutputType))
    (appTest sessionId appType testDataType testInputType testOutputType)

-- Build a shell command represented by string and run the command as a
-- subprocess, where the command is a cohort-building application. If the
-- application writes the results to S3 then copy those results back to the
-- local filesystem
appTest :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> IO ()
appTest sessionId appType testDataType testInputType testOutputType = do
  let outfilename = createFilenameForResults appType testDataType testInputType testOutputType
  let isS3out = case testOutputType of
        TestOutputS3 -> True
        _ -> False
  let cmd = appTestCmd sessionId appType testDataType testInputType testOutputType
  print $ "TEST COMMAND:  " ++ cmd
  pure cmd >>= callCommand
  when isS3out $ copyResultsFromS3 sessionId outfilename

-- Construct a string representing a shell command that runs one of the testing
-- cohort-building applications on the test data
--
-- Note that if the input is specified as coming from standard input, then a
-- fragment like `"< /path/to/file"` is inserted in the middle of the command
-- string. While this is not usual practice, the shell removes the fragment
-- prior to processing and things do indeed work as intended
appTestCmd :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> String
appTestCmd sessionId appType testDataType testInputType testOutputType =
  appCmd ++ " " ++ inputFragm ++ " " ++ outputFragm
  where
    infilename = createLocalFilepathForTest testDataType
    outfilename = createFilenameForResults appType testDataType testInputType testOutputType
    appCmd = case appType of
        AppRowWise -> "exampleAppRW"
        AppColumnWise -> "exampleAppCW"
    inputFragm = case testInputType of
        TestInputFile -> "-f " ++ infilename
        TestInputStdin -> "< " ++ infilename
        TestInputS3 -> "-r us-east-1 -b " ++ s3Bucket ++ " -k " ++ createS3keyforTest sessionId testDataType
    outputFragm = case testOutputType of
        TestOutputFile -> "-o " ++ createLocalFilepathForResults outfilename
        TestOutputStdout -> "> " ++ createLocalFilepathForResults outfilename
        TestOutputS3 -> "--outregion us-east-1 --outbucket " ++ s3Bucket ++ " --outkey " ++ createS3keyForResults sessionId outfilename

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

-- Copy test data to S3
writeTestDataToS3 :: String -> TestDataType -> IO ()
writeTestDataToS3 sessionId testDataType = pure cmd >>= callCommand where
  from = createLocalFilepathForTest testDataType
  to   = createS3uriForTest $ createS3keyforTest sessionId testDataType
  cmd  = "aws s3 cp " ++ from ++ " " ++ to

-- Copy results from S3
copyResultsFromS3 :: String -> String -> IO ()
copyResultsFromS3 sessionId filename =
  pure cmd >>= callCommand where
    uri = createS3uriForTest $ createS3keyForResults sessionId filename
    cmd = "aws s3 cp " ++ uri ++ " " ++ createLocalFilepathForResults filename

-- Delete results from S3
removeSessionDirFromS3 :: String -> String -> IO ()
removeSessionDirFromS3 prefix sessionId =
  pure cmd >>= callCommand where
    fileglob = prefix ++ sessionId
    uri      = createS3uriForTest fileglob
    cmd      = "aws s3 rm --recursive " ++ uri

-- Create the local filepath where the test data is stored
createLocalFilepathForTest :: TestDataType -> String
createLocalFilepathForTest TestDataEmpty = localTestDataDir ++ "testEmptyData.jsonl"
createLocalFilepathForTest TestDataSmall = localTestDataDir ++ "testData.jsonl"
createLocalFilepathForTest TestDataManySubj = localTestDataDir ++ "testmanysubjects.jsonl"
createLocalFilepathForTest TestDataManyEvent = localTestDataDir ++ "testmanyevents.jsonl"

-- Helper function to create the local filpath from a filename
createLocalFilepathForResults :: String -> String
createLocalFilepathForResults = (localResultsDir ++)

-- Create the S3 key where the test data will be located (once paired with a bucket)
createS3keyforTest :: String -> TestDataType -> String
createS3keyforTest sessionId TestDataEmpty = s3TestDataDir ++ sessionId ++ "/testdata-empty.jsonl"
createS3keyforTest sessionId TestDataSmall = s3TestDataDir ++ sessionId ++ "/testdata-small.jsonl"
createS3keyforTest sessionId TestDataManySubj = s3TestDataDir ++ sessionId ++ "/testdata-manysubj.jsonl"
createS3keyforTest sessionId TestDataManyEvent = s3TestDataDir ++ sessionId ++ "/testdata-manyevent.jsonl"

-- Create the S3 key where the results will be located (once paired with a bucket)
createS3keyForResults :: String -> String -> String
createS3keyForResults sessionId filename = s3ResultsDir ++ sessionId ++ "/" ++ filename

-- Create the S3 URI where the test data will be located
createS3uriForTest  :: String -> String
createS3uriForTest = (("s3://" ++ s3Bucket ++ "/") ++)

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

-- Construct the local filepath where the golden file is found for a given test
createLocalFilepathForGolden :: AppType -> TestDataType -> String
createLocalFilepathForGolden appType testDataType = concat
  [ localTestDataDir
  , "test"
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

-- Construct the filename for the output for a given test
createFilenameForResults :: AppType -> TestDataType -> TestInputType -> TestOutputType -> String
createFilenameForResults appType testDataType testInputType testOutputType = concat
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
