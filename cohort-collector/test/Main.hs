{-| Tests of the collector application
-}

module Main
  ( main
  ) where

import           CohortCollectionTests
import           ConstructPaths
import           Control.Exception           (catch, throwIO)
import           System.Directory            (createDirectoryIfMissing,
                                              removeDirectoryRecursive,
                                              removePathForcibly)
import           System.Exit                 (ExitCode)
import           System.IO.Temp              (writeSystemTempFile)
import           Test.Tasty                  (TestTree, defaultMain, testGroup)
import           TestUtils.ConstructTestTree
import           TestUtils.S3Utils
import           TestUtils.SessionId
import           TestUtils.TestCases

main = testsMain


-- FIXME:
-- 2022-04-14 - BS -
-- This is test suite is commented out for now
-- as the test execution is failing on in the CI
-- due to errors such as:
--   exampleFilterApp: MissingFileError "/root/.aws/credentials"
-- punting this to resolve another day.
-- see: https://gitlab.novisci.com/nsStat/asclepias/-/issues/205

-- main :: IO ()
-- main = do

--   -- Generate a unique session ID and ensure that the results directory exists
--   sessionId <- getSessionId
--   createDirectoryIfMissing True localResultsDir

--   -- Copy the test data to S3 with session-specific keys to avoid collisions
--   let testDataFilesCw = ["testcw1.json", "testcw2.json", "testcw3.json"]
--   let testDataFilesRw = ["testrw1.json", "testrw2.json", "testrw3.json"]
--   mapM_ (writeTestDataToS3 sessionId) (testDataFilesCw ++ testDataFilesRw)

--   -- Write the S3-specific manifests to disk
--   s3manifestCwFilepath <- writeSystemTempFile
--     "s3manifestcw.txt"
--     (unlines (map (convFilenameToS3KeyTest sessionId) testDataFilesCw))
--   s3manifestRwFilepath <- writeSystemTempFile
--     "-s3manifestrw.txt"
--     (unlines (map (convFilenameToS3KeyTest sessionId) testDataFilesRw))

--   -- Copy the S3-specific manifests to S3
--   s3Copy s3manifestCwFilepath
--          (convFilenameToS3UriTest sessionId "manifestcw.txt")
--   s3Copy s3manifestRwFilepath
--          (convFilenameToS3UriTest sessionId "manifestrw.txt")

--   -- Create a TestTree of I/O tests
--   let testsIO = createCollectorTests "Tests of cohort collection (IO)"
--                                      (appGoldenVsFileDrv sessionId)

--   -- Run the tests and perform cleanup. Note that ANY CODE WRITTEN AFTER THIS
--   -- EXPRESION WILL BE SILENTLY IGNORED
--   defaultMain (testGroup "cohort-collector tests" [tests, testsIO])
--     `catch` (\e -> do
--               removeDirectoryRecursive localResultsDir
--               removePathForcibly s3manifestCwFilepath
--               removePathForcibly s3manifestRwFilepath
--               s3RecursiveRm (convS3KeyToUri (s3RootDir ++ sessionId))
--               throwIO (e :: ExitCode)
--             )

-- appGoldenVsFileDrv :: String -> TestCollectorScenario -> TestTree
-- appGoldenVsFileDrv sessionId = appGoldenVsFile constructCollectorTestName
--                                                constructFilepathForTest
--                                                constructFilepathForGolden
--                                                constructFilepathForResult
--                                                (appTestDrv sessionId)

-- appTestDrv :: String -> TestCollectorScenario -> IO ()
-- appTestDrv sessionId = appTest (\_ -> pure ())
--                                (appTestCmdDrv sessionId)
--                                (postCollectorCmdHookS3Drv sessionId)

-- appTestCmdDrv :: String -> TestCollectorScenario -> IO ()
-- appTestCmdDrv sessionId = appTestCmd (appTestCmdStringDrv sessionId)

-- appTestCmdStringDrv :: String -> TestCollectorScenario -> String
-- appTestCmdStringDrv sessionId = appTestCmdString
--   (const "cabal run --verbose=0 cohort-collector --")
--   (constructTestInputFragmDrv sessionId)
--   (constructTestOutputFragmDrv sessionId)

-- constructTestInputFragmDrv :: String -> TestCollectorScenario -> String
-- constructTestInputFragmDrv sessionId = constructTestCollectorInputFragm
--   constructFilepathForTest
--   constructBucketForTest
--   (constructS3KeyForTest sessionId)

-- constructTestOutputFragmDrv :: String -> TestCollectorScenario -> String
-- constructTestOutputFragmDrv sessionId = constructTestCollectorOutputFragm
--   constructFilepathForResult
--   (const s3Bucket)
--   (constructS3KeyForResult sessionId)

-- postCollectorCmdHookS3Drv :: String -> TestCollectorScenario -> IO ()
-- postCollectorCmdHookS3Drv sessionId = postCollectorCmdHookS3
--   (constructS3UriForResult sessionId)
--   constructFilepathForResult

-- constructCollectorTestName :: TestCollectorScenario -> String
-- constructCollectorTestName testCollectorScenario =
--   "cohort-collector app of a "
--     ++ case getTestCollectorAppType testCollectorScenario of
--          AppRowWise    -> "row-wise"
--          AppColumnWise -> "column-wise"
--     ++ " cohort reading from "
--     ++ case getTestCollectorInputType testCollectorScenario of
--          TestCollectorInputFile -> "file"
--          TestCollectorInputS3   -> "S3"
--     ++ " and writing to "
--     ++ case getTestCollectorOutputType testCollectorScenario of
--          TestCollectorOutputFile   -> "file"
--          TestCollectorOutputStdout -> "standard output"
--          TestCollectorOutputS3     -> "S3"

-- writeTestDataToS3 :: String -> String -> IO ()
-- writeTestDataToS3 sessionId filename = s3Copy from to
--  where
--   from = convFilenameToFilepathTest filename
--   to   = convFilenameToS3UriTest sessionId filename
