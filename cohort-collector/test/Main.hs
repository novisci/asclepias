{-| Tests of the collector application
-}

module Main
  ( main
  ) where

import           CohortCollectionTests
import           TestUtils.TestCases
import           TestUtils.ConstructTestTree

main :: IO ()
main = testsMain

localTestDataDir :: String
localTestDataDir = "test/tests/"

localResultsDir :: String
localResultsDir = "test/results/"

s3Bucket :: String
s3Bucket = "download.novisci.com"

s3RootDir :: String
s3RootDir = "hasklepias/sandbox-testapps/collectorApp/"


appTest' :: String -> TestCollectorScenario -> IO ()
appTest' sessionId =
  appTest
    (\_ -> pure ())
    (appTestCmd' sessionId)
    (postCollectorCmdHookS3' sessionId)

appTestCmd' :: String -> TestCollectorScenario -> IO ()
appTestCmd' sessionId = appTestCmd (appTestCmdString' sessionId)

appTestCmdString' :: String -> TestCollectorScenario -> String
appTestCmdString' sessionId =
  appTestCmdString
    (const "cohort-collector")
    (constructTestInputFragm' sessionId)
    (constructTestOutputFragm' sessionId)

constructTestInputFragm' :: String -> TestCollectorScenario -> String
constructTestInputFragm' sessionId =
  constructTestCollectorInputFragm
    constructFilepathForTest
    constructBucketForTest
    (constructS3KeyForTest sessionId)

constructTestOutputFragm' :: String -> TestCollectorScenario -> String
constructTestOutputFragm' sessionId =
  constructTestCollectorOutputFragm
    constructFilepathForResult
    (const s3Bucket)
    (constructS3KeyForResult sessionId)

postCollectorCmdHookS3' :: String -> TestCollectorScenario -> IO ()
postCollectorCmdHookS3' sessionId =
  postCollectorCmdHookS3
    (constructS3UriForResult sessionId)
    constructFilepathForResult

constructFilenameForTestBase :: AppType -> FilePath
constructFilenameForTestBase AppRowWise = "testrw.locations"
constructFilenameForTestBase AppColumnWise = "testcw.locations"

constructFilenameForTest :: TestCollectorScenario -> FilePath
constructFilenameForTest =
  constructFilenameForTestBase . getTestCollectorAppType

constructFilenameForResult :: TestCollectorScenario -> String
constructFilenameForResult testCollectorScenario =
  "results-"
    ++ case getTestCollectorAppType testCollectorScenario of
        AppRowWise -> "rw"
        AppColumnWise -> "cw"
    ++ "-"
    ++ case getTestCollectorInputType testCollectorScenario of
        TestCollectorInputFile -> "filein"
        TestCollectorInputS3 -> "s3in"
    ++ "-"
    ++ case getTestCollectorOutputType testCollectorScenario of
        TestCollectorOutputFile -> "fileout"
        TestCollectorOutputStdout -> "stdout"
        TestCollectorOutputS3 -> "s3out"
    ++ ".json"

constructFilepathForTestBase :: AppType -> FilePath
constructFilepathForTestBase =
  (localTestDataDir ++) . constructFilenameForTestBase

constructFilepathForTest :: TestCollectorScenario -> FilePath
constructFilepathForTest = constructFilepathForTestBase . getTestCollectorAppType

constructFilepathForResult :: TestCollectorScenario -> FilePath
constructFilepathForResult =
  (localResultsDir ++) . constructFilenameForResult

constructBucketForTest :: TestCollectorScenario -> String
constructBucketForTest = const s3Bucket

constructS3KeyForTest :: String -> TestCollectorScenario -> FilePath
constructS3KeyForTest sessionId testCollectorScenario =
  convFilenameToS3KeyTest
    sessionId
    (constructFilenameForTest testCollectorScenario)

constructS3KeyForResult :: String -> TestCollectorScenario -> FilePath
constructS3KeyForResult sessionId testCollectorScenario =
  convFilenameToS3KeyResult
    sessionId
    (constructFilenameForResult testCollectorScenario)

constructS3UriForResult :: String -> TestCollectorScenario -> FilePath
constructS3UriForResult sessionId testCollectorScenario =
  convS3KeyToUri
    (constructS3KeyForResult sessionId testCollectorScenario)

convFilenameToS3KeyTest :: String -> String -> String
convFilenameToS3KeyTest sessionId filename =
  s3RootDir ++ sessionId ++ "/testdata/" ++ filename

convFilenameToS3KeyResult :: String -> String -> String
convFilenameToS3KeyResult sessionId filename =
  s3RootDir ++ sessionId ++ "/results/" ++ filename

convS3KeyToUri :: String -> String
convS3KeyToUri s3Key = "s3://" ++ s3Bucket ++ "/" ++ s3Key
