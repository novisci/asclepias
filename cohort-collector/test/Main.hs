{-| Tests of the collector application
-}

module Main
  ( main
  ) where

import           CohortCollectionTests
import           System.Directory               ( createDirectoryIfMissing
                                                , removeDirectoryRecursive
                                                , removePathForcibly
                                                )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           TestUtils.SessionId
import           TestUtils.TestCases
import           TestUtils.ConstructTestTree

main :: IO ()
main = do

  -- Generate a unique session ID and ensure that the results directory exists
  sessionId <- getSessionId
  createDirectoryIfMissing True localResultsDir

  -- -- Copy the test data to S3 with session-specific keys to avoid collisions
  -- writeTestDataToS3 sessionId TestDataEmpty
  -- writeTestDataToS3 sessionId TestDataSmall
  -- writeTestDataToS3 sessionId TestDataManySubj
  -- writeTestDataToS3 sessionId TestDataManyEvent

  let testsIO =
        createCollectorTests
          "Tests of cohort collection (IO)"
          (appGoldenVsFile' sessionId)

  -- Run the tests and perform cleanup. Note that ANY CODE WRITTEN AFTER THIS
  -- EXPRESION WILL BE SILENTLY IGNORED
  defaultMain (testGroup "cohort-collector tests" [tests, testsIO])

localTestDataDir :: String
localTestDataDir = "test/tests/"

localResultsDir :: String
localResultsDir = "test/results/"

s3Bucket :: String
s3Bucket = "download.novisci.com"

s3RootDir :: String
s3RootDir = "hasklepias/sandbox-testapps/collectorApp/"

appGoldenVsFile' :: String -> TestCollectorScenario -> TestTree
appGoldenVsFile' sessionId =
  appGoldenVsFile
    constructCollectorTestName
    constructFilepathForTest
    constructFilepathForGolden
    constructFilepathForResult
    (appTest' sessionId)

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

constructFilenameForGolden :: TestCollectorScenario -> FilePath
constructFilenameForGolden testCollectorScenario =
  -- constructFilenameForTestBase . getTestCollectorAppType
  localTestDataDir ++ filename
  where
    filename =
      case getTestCollectorAppType testCollectorScenario of
        AppRowWise -> "testrw.golden"
        AppColumnWise -> "testcw.golden"

constructFilepathForTestBase :: AppType -> FilePath
constructFilepathForTestBase =
  (localTestDataDir ++) . constructFilenameForTestBase

constructFilepathForTest :: TestCollectorScenario -> FilePath
constructFilepathForTest = constructFilepathForTestBase . getTestCollectorAppType

constructFilepathForResult :: TestCollectorScenario -> FilePath
constructFilepathForResult =
  (localResultsDir ++) . constructFilenameForResult

constructFilepathForGolden :: TestCollectorScenario -> FilePath
constructFilepathForGolden =
  (localResultsDir ++) . constructFilenameForGolden

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

constructCollectorTestName :: TestCollectorScenario -> String
constructCollectorTestName testCollectorScenario =
  "cohort-collector app of a "
    ++ case getTestCollectorAppType testCollectorScenario of
         AppRowWise-> "row-wise"
         AppColumnWise -> "column-wise"
    ++ " cohort reading from "
    ++ case getTestCollectorInputType testCollectorScenario of
         TestCollectorInputFile -> "file"
         TestCollectorInputS3 -> "S3"
    ++ " and writing to "
    ++ case getTestCollectorOutputType testCollectorScenario of
         TestCollectorOutputFile -> "file"
         TestCollectorOutputStdout -> "standard output"
         TestCollectorOutputS3 -> "S3"
