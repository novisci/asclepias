-- |

module ConstructPaths
  ( createFilenameForGolden
  , createFilenameForResult
  , createFilenameForTest
  , createFilepathForGolden
  , createFilepathForResult
  , createFilepathForTest
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

-- Create the local filepath where the test data is stored
createFilenameForTest :: TestDataType -> String
createFilenameForTest TestDataEmpty = "testEmptyData.jsonl"
createFilenameForTest TestDataSmall = "testData.jsonl"
createFilenameForTest TestDataManySubj = "testmanysubjects.jsonl"
createFilenameForTest TestDataManyEvent = "testmanyevents.jsonl"

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
    ++ createFilenameForTest (getCohortTestDataType testScenarioCohort)

-- Helper function to create the local filepath based on a test scenario
createFilepathForResult :: TestScenarioCohort -> String
createFilepathForResult testScenarioCohort =
  localResultsDir ++ createFilenameForResult testScenarioCohort

createFilepathForGolden :: TestScenarioCohort -> String
createFilepathForGolden testScenarioCohort =
  localTestDataDir ++ createFilenameForGolden testScenarioCohort
