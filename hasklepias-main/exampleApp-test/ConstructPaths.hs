-- |

module ConstructPaths
  ( convFilenameToFilepathResult
  , convFilenameToFilepathTest
  , convFilenameToS3KeyResult
  , convFilenameToS3KeyTest
  , convFilenameToS3UriResult
  , convFilenameToS3UriTest
  , createFilenameForGolden
  , createFilenameForResult
  , createFilenameForTest
  , createFilenameForTestBase
  , createFilepathForGolden
  , createFilepathForResult
  , createFilepathForTest
  , createS3KeyForResult
  , createS3KeyForTest
  , createS3UriForTest
  , createS3UriForResult
  , localResultsDir
  , localTestDataDir
  , s3Bucket
  , s3RootDir
  ) where

import           TestUtils.TestCases

localTestDataDir :: String
localTestDataDir = "exampleApp-test/test/"

localResultsDir :: String
localResultsDir = "exampleApp-test/results/"

s3Bucket :: String
s3Bucket = "download.novisci.com"

s3RootDir :: String
s3RootDir = "hasklepias/sandbox-testapps/cohortApp/"

-- -- Create the local filepath where the test data is stored
createFilenameForTestBase :: TestDataType -> String
createFilenameForTestBase TestDataEmpty = "testEmptyData.jsonl"
createFilenameForTestBase TestDataSmall = "testData.jsonl"
createFilenameForTestBase TestDataManySubj = "testmanysubjects.jsonl"
createFilenameForTestBase TestDataManyEvent = "testmanyevents.jsonl"

-- Create the local filepath where the test data is stored
createFilenameForTest :: TestScenarioCohort  -> String
createFilenameForTest testScenarioCohort =
  createFilenameForTestBase (getCohortTestDataType testScenarioCohort)

-- Construct the filename for the output for a given test
createFilenameForResult :: TestScenarioCohort -> String
createFilenameForResult testScenarioCohort = concat
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

-- Construct the local filepath where the golden file is found for a given test
createFilenameForGolden :: TestScenarioCohort -> String
createFilenameForGolden testScenarioCohort =
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

createFilepathForTest ::  TestScenarioCohort -> String
createFilepathForTest testScenarioCohort =
  localTestDataDir
    ++ createFilenameForTest testScenarioCohort

-- Helper function to create the local filepath based on a test scenario
createFilepathForResult :: TestScenarioCohort -> String
createFilepathForResult testScenarioCohort =
  localResultsDir ++ createFilenameForResult testScenarioCohort

createFilepathForGolden :: TestScenarioCohort -> String
createFilepathForGolden testScenarioCohort =
  localTestDataDir ++ createFilenameForGolden testScenarioCohort

createS3KeyForTest :: String -> TestScenarioCohort -> String
createS3KeyForTest sessionId testScenarioCohort =
  convFilenameToS3KeyTest
    sessionId
    (createFilenameForTest testScenarioCohort)

createS3KeyForResult :: String -> TestScenarioCohort -> String
createS3KeyForResult sessionId testScenarioCohort =
  convFilenameToS3KeyResult sessionId (createFilenameForResult testScenarioCohort)

createS3UriForTest :: String -> TestScenarioCohort -> String
createS3UriForTest sessionId testScenarioCohort =
  convFilenameToS3UriTest sessionId (createFilenameForTest testScenarioCohort)

createS3UriForResult :: String -> TestScenarioCohort -> String
createS3UriForResult sessionId testScenarioCohort =
  convFilenameToS3UriResult sessionId (createFilenameForResult testScenarioCohort)

convFilenameToFilepathTest :: String -> String
convFilenameToFilepathTest = (localTestDataDir ++)

convFilenameToFilepathResult :: String -> String
convFilenameToFilepathResult = (localResultsDir ++)

-- Create the S3 key where the test data will be located (once paired with a
-- bucket)
convFilenameToS3KeyTest :: String -> String -> String
convFilenameToS3KeyTest sessionId filename =
  s3RootDir ++ sessionId ++ "/testdata/" ++ filename

-- Create the S3 key where the results will be located (once paired with a
-- bucket)
convFilenameToS3KeyResult :: String -> String -> String
convFilenameToS3KeyResult sessionId filename =
  s3RootDir ++ sessionId ++ "/results/" ++ filename

-- Create the S3 key where the test data will be located (once paired with a
-- bucket)
convFilenameToS3UriTest :: String -> String -> String
convFilenameToS3UriTest sessionId filename = convS3KeyToUri $ convFilenameToS3KeyTest sessionId filename

-- Create the S3 key where the test data will be located (once paired with a
-- bucket)
convFilenameToS3UriResult :: String -> String -> String
convFilenameToS3UriResult sessionId filename = convS3KeyToUri $ convFilenameToS3KeyResult sessionId filename

convS3KeyToUri :: String -> String
convS3KeyToUri s3Key = "s3://" ++ s3Bucket ++ "/" ++ s3Key
