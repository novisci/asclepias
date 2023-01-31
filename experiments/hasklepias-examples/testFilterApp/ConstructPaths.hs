module ConstructPaths
  ( createFilenameForTest
  , createFilenameForGolden
  , createFilenameForResults
  , createFilepathForTest
  , createFilepathForGolden
  , createFilepathForResults
  , createS3KeyForTest
  , createS3UriForTest
  , localResultsDir
  , localTestDataDir
  , s3Bucket
  , s3TestDataDir
  ) where

import           TestUtils.BuildLargeTestData (largeInputSize)
import           TestUtils.TestCases          (TestInputType (..))

-- Data constants --------------------------------------------------------------

localTestDataDir :: String
localTestDataDir = "testFilterApp/test/"

localResultsDir :: String
localResultsDir = "testFilterApp/results/"

s3Bucket :: String
s3Bucket = "download.novisci.com"

s3TestDataDir :: String
s3TestDataDir = "hasklepias/sandbox-testapps/filterApp/"


-- File location routines ------------------------------------------------------

createFilenameForTest :: String -> String
createFilenameForTest id = "test-" ++ id ++ ".jsonl"

createFilenameForGolden :: String -> String
createFilenameForGolden id = "test-" ++ id ++ ".golden"

-- Construct the filename for the output for a given test
createFilenameForResults :: String -> TestInputType -> String
createFilenameForResults id testInputType =
  "result-" ++ id ++ "-" ++ inputStr ++ ".jsonl"
 where
  inputStr = case testInputType of
    TestInputFile  -> "filein"
    TestInputStdin -> "stdin"
    TestInputS3    -> "s3in"

createFilepathForTest :: String -> String
createFilepathForTest id = localTestDataDir ++ createFilenameForTest id

createFilepathForGolden :: String -> String
createFilepathForGolden id = localTestDataDir ++ createFilenameForGolden id

createFilepathForResults :: String -> TestInputType -> String
createFilepathForResults id testInputType =
  localResultsDir ++ createFilenameForResults id testInputType

-- Create the S3 key where the test data will be located (once paired with a bucket)
createS3KeyForTest :: String -> String -> String
createS3KeyForTest sessionId id =
  s3TestDataDir ++ sessionId ++ "/testdata/" ++ createFilenameForTest id

-- Create the S3 URI where the test data will be located
createS3UriForTest sessionId id =
  "s3://" ++ s3Bucket ++ "/" ++ createS3KeyForTest sessionId id
