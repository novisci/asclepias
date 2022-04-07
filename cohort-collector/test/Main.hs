{-| Tests of the collector application
-}

module Main
  ( main
  ) where

import           CohortCollectionTests
import           ConstructPaths
import           Control.Exception             ( catch
                                               , throwIO )
import           System.Directory               ( createDirectoryIfMissing
                                                , removeDirectoryRecursive
                                                , removePathForcibly
                                                )
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
  let testDataFiles =
        [ "testcw.locations", "testcw1.json", "testcw2.json", "testcw3.json"
        , "testrw.locations", "testrw1.json", "testrw2.json", "testrw3.json"
        ]
  mapM_ (writeTestDataToS3 sessionId) testDataFiles

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
