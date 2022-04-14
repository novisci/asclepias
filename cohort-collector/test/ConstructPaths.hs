-- |

module ConstructPaths
  ( constructFilenameForTestBase
  , constructFilenameForTest
  , constructFilenameForResult
  , constructFilenameForGolden
  , constructFilepathForTestBase
  , constructFilepathForTest
  , constructFilepathForResult
  , constructFilepathForGolden
  , constructBucketForTest
  , constructS3KeyForTest
  , constructS3KeyForResult
  , constructS3UriForResult
  , convFilenameToFilepathTest
  , convFilenameToS3KeyTest
  , convFilenameToS3UriTest
  , convFilenameToS3KeyResult
  , convS3KeyToUri
  , localResultsDir
  , localTestDataDir
  , s3Bucket
  , s3RootDir
  ) where

import           TestUtils.TestCases

-- Data constants --------------------------------------------------------------

localTestDataDir :: String
localTestDataDir = "test/tests/"

localResultsDir :: String
localResultsDir = "test/results/"

s3Bucket :: String
s3Bucket = "download.novisci.com"

s3RootDir :: String
s3RootDir = "hasklepias/sandbox-testapps/collectorApp/"


-- File location routines ------------------------------------------------------

constructFilenameForTestBase :: AppType -> FilePath
constructFilenameForTestBase AppRowWise    = "manifestrw.txt"
constructFilenameForTestBase AppColumnWise = "manifestcw.txt"

constructFilenameForTest :: TestCollectorScenario -> FilePath
constructFilenameForTest =
  constructFilenameForTestBase . getTestCollectorAppType

constructFilenameForResult :: TestCollectorScenario -> String
constructFilenameForResult testCollectorScenario =
  "results-"
    ++ case getTestCollectorAppType testCollectorScenario of
         AppRowWise    -> "rw"
         AppColumnWise -> "cw"
    ++ "-"
    ++ case getTestCollectorInputType testCollectorScenario of
         TestCollectorInputFile -> "filein"
         TestCollectorInputS3   -> "s3in"
    ++ "-"
    ++ case getTestCollectorOutputType testCollectorScenario of
         TestCollectorOutputFile   -> "fileout"
         TestCollectorOutputStdout -> "stdout"
         TestCollectorOutputS3     -> "s3out"
    ++ ".json"

constructFilenameForGolden :: TestCollectorScenario -> FilePath
constructFilenameForGolden testCollectorScenario =
  case getTestCollectorAppType testCollectorScenario of
    AppRowWise    -> "testrw.golden"
    AppColumnWise -> "testcw.golden"

constructFilepathForTestBase :: AppType -> FilePath
constructFilepathForTestBase =
  (localTestDataDir ++) . constructFilenameForTestBase

constructFilepathForTest :: TestCollectorScenario -> FilePath
constructFilepathForTest =
  constructFilepathForTestBase . getTestCollectorAppType

constructFilepathForResult :: TestCollectorScenario -> FilePath
constructFilepathForResult = (localResultsDir ++) . constructFilenameForResult

constructFilepathForGolden :: TestCollectorScenario -> FilePath
constructFilepathForGolden = (localTestDataDir ++) . constructFilenameForGolden

constructBucketForTest :: TestCollectorScenario -> String
constructBucketForTest = const s3Bucket

constructS3KeyForTest :: String -> TestCollectorScenario -> String
constructS3KeyForTest sessionId testCollectorScenario = convFilenameToS3KeyTest
  sessionId
  (constructFilenameForTest testCollectorScenario)

constructS3KeyForResult :: String -> TestCollectorScenario -> String
constructS3KeyForResult sessionId testCollectorScenario =
  convFilenameToS3KeyResult sessionId
                            (constructFilenameForResult testCollectorScenario)

constructS3UriForResult :: String -> TestCollectorScenario -> String
constructS3UriForResult sessionId testCollectorScenario =
  convS3KeyToUri (constructS3KeyForResult sessionId testCollectorScenario)

convFilenameToFilepathTest :: FilePath -> FilePath
convFilenameToFilepathTest = (localTestDataDir ++)

convFilenameToS3KeyTest :: String -> String -> String
convFilenameToS3KeyTest sessionId filename =
  s3RootDir ++ sessionId ++ "/testdata/" ++ filename

convFilenameToS3UriTest :: String -> String -> String
convFilenameToS3UriTest sessionId =
  convS3KeyToUri . convFilenameToS3KeyTest sessionId

convFilenameToS3KeyResult :: String -> String -> String
convFilenameToS3KeyResult sessionId filename =
  s3RootDir ++ sessionId ++ "/results/" ++ filename

convS3KeyToUri :: String -> String
convS3KeyToUri s3Key = "s3://" ++ s3Bucket ++ "/" ++ s3Key
