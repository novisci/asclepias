{-| Tests of the collector application
-}

module Main
  ( main
  ) where

import           CohortCollectionTests
import           ConstructPaths
import           Control.Exception              ( catch
                                                , throwIO )
import           System.Directory               ( createDirectoryIfMissing
                                                , removeDirectoryRecursive
                                                , removePathForcibly
                                                )
import           System.IO.Temp                 ( writeSystemTempFile )
import           System.Exit                    ( ExitCode )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           TestUtils.ConstructTestTree
import           TestUtils.S3Utils
import           TestUtils.SessionId
import           TestUtils.TestCases

main :: IO ()
main = do

  -- Generate a unique session ID and ensure that the results directory exists
  sessionId <- getSessionId
  createDirectoryIfMissing True localResultsDir

  -- Copy the test data to S3 with session-specific keys to avoid collisions
  let testDataFilesCw =
        ["testcw1.json", "testcw2.json", "testcw3.json"]
  let testDataFilesRw =
        ["testrw1.json", "testrw2.json", "testrw3.json"]
  mapM_ (writeTestDataToS3 sessionId) (testDataFilesCw ++ testDataFilesRw)

  -- Write the S3-specific manifests to disk
  s3manifestCwFilepath <- writeSystemTempFile
    "s3manifestcw.txt"
    (unlines (map (convFilenameToS3KeyTest sessionId) testDataFilesCw))
  s3manifestRwFilepath <- writeSystemTempFile
    "-s3manifestrw.txt"
    (unlines (map (convFilenameToS3KeyTest sessionId) testDataFilesRw))

  -- Copy the S3-specific manifests to S3
  s3Copy
    s3manifestCwFilepath
    (convFilenameToS3UriTest sessionId "manifestcw.txt")
  s3Copy
    s3manifestRwFilepath
    (convFilenameToS3UriTest sessionId "manifestrw.txt")

  -- Create a TestTree of I/O tests
  let testsIO =
        createCollectorTests
          "Tests of cohort collection (IO)"
          (appGoldenVsFile' sessionId)

  -- Run the tests and perform cleanup. Note that ANY CODE WRITTEN AFTER THIS
  -- EXPRESION WILL BE SILENTLY IGNORED
  defaultMain (testGroup "cohort-collector tests" [tests, testsIO])
    `catch` (\e -> do
      removeDirectoryRecursive localResultsDir
      removePathForcibly s3manifestCwFilepath
      removePathForcibly s3manifestRwFilepath
      s3RecursiveRm  (convS3KeyToUri (s3RootDir ++ sessionId))
      throwIO (e :: ExitCode))

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
    (const "cabal run --verbose=0 cohort-collector --")
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

writeTestDataToS3 :: String -> String -> IO ()
writeTestDataToS3 sessionId filename =
  s3Copy from to
  where
    from = convFilenameToFilepathTest filename
    to = convFilenameToS3UriTest sessionId filename
