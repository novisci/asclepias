{-| Tests of the example application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           ConstructPaths
import           Control.Monad                  ( when )  -- FIXME after removing unneeded code using this
import           Control.Exception             ( catch
                                               , throwIO )
import qualified Data.ByteString.Lazy          as B
import           Data.Char                     ( isDigit )
import           Hasklepias
import           TestUtils.BuildLargeTestData
import           Hasklepias.ExampleApp
import           TestUtils.ConstructTestTree
import           TestUtils.SessionId
import           TestUtils.TestCases
import           TestUtils.S3Utils                        -- FIXME after removing unneeded code using this
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
import TestUtils.TestCases (AppType(AppColumnWise, AppRowWise))

-- Run the tests
--
-- With regards to test data generation: note that changing the number of events
-- for the "many events" test data doesn't change the output of the cohort
-- creation (since the cohort definition throws that data away anyway), so for
-- this reason we create a golden file and store it in the repository. On the
-- other hand, for the "many subjects" test data do we generate the data and
-- corresponding golden file each time we do the testing so as to (i) prevent
-- storing large files in the repository, and (ii) so that we can change the
-- number of test subjects with ease.
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
  defaultMain (createTestsCartesian "Cohort creation tests" (appGoldenVsFile' sessionId))
    `catch` (\e -> do
      removeDirectoryRecursive localResultsDir
      s3RecursiveRm (convS3KeyToUri s3RootDir)
      removePathForcibly (createFilepathForTestBase TestDataManyEvent)
      removePathForcibly (createFilepathForTestBase TestDataManySubj)
      removePathForcibly (createFilepathForGoldenBase AppColumnWise TestDataManySubj)
      removePathForcibly (createFilepathForGoldenBase AppRowWise TestDataManySubj)
      throwIO (e :: ExitCode))

appGoldenVsFile' :: String -> TestScenarioCohort -> TestTree
appGoldenVsFile' sessionId =
  appGoldenVsFile
    constructTestName
    createFilepathForTest
    createFilepathForGolden
    createFilepathForResult
    (appTest' sessionId)

appTest' :: String -> TestScenarioCohort -> IO ()
appTest' sessionId =
  appTest
    (\_ -> pure ())
    (appTestCmd' sessionId)
    (postCmdHookS3 (createS3UriForResult sessionId) createFilepathForResult)

appTestCmd' :: String -> TestScenarioCohort -> IO ()
appTestCmd' sessionId = appTestCmd (appTestCmdString' sessionId)

appTestCmdString' :: String -> TestScenarioCohort -> String
appTestCmdString' sessionId =
  appTestCmdString
    constructTestExecutableFragm
    (constructTestInputFragmFSS' sessionId)
    (constructTestOutputFragmFSS' sessionId)

constructTestExecutableFragm :: TestScenarioCohort -> String
constructTestExecutableFragm testScenarioCohort =
  case getCohortAppType testScenarioCohort of
    AppRowWise -> "exampleAppRW"
    AppColumnWise -> "exampleAppCW"

constructTestInputFragmFSS' :: String -> TestScenarioCohort -> String
constructTestInputFragmFSS' sessionId =
  constructTestInputFragmFSS
    createFilepathForTest
    (const s3Bucket)
    (createS3KeyForTest sessionId)

constructTestOutputFragmFSS' :: String -> TestScenarioCohort -> String
constructTestOutputFragmFSS' sessionId =
  constructTestOutputFragmFSS
    createFilepathForResult
    (const s3Bucket)
    (createS3KeyForResult sessionId)

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

constructTestName :: TestScenarioCohort -> String
constructTestName testScenarioCohort =
  "ExampleApp of a "
    ++ case getCohortAppType testScenarioCohort of
         AppRowWise-> "row-wise"
         AppColumnWise -> "column-wise"
    ++ " getCohortCohort performed on "
    ++ case getCohortTestDataType testScenarioCohort of
         TestDataEmpty -> "empty data"
         TestDataSmall -> "small data"
         TestDataManySubj -> "many subjects data"
         TestDataManyEvent -> "many events data"
    ++ " reading from "
    ++ case getCohortTestInputType testScenarioCohort of
         TestInputFile -> "file"
         TestInputStdin -> "standard input"
         TestInputS3 -> "S3"
    ++ " and writing to "
    ++ case getCohortTestOutputType testScenarioCohort of
         TestOutputFile -> "file"
         TestOutputStdout -> "standard output"
         TestOutputS3 -> "S3"

-- Copy local test data to S3
writeTestDataToS3 :: String -> TestDataType -> IO ()
writeTestDataToS3 sessionId testDataType =
  s3Copy from to
  where
    filename = createFilenameForTestBase testDataType
    from = convFilenameToFilepathTest filename
    to = convFilenameToS3UriTest sessionId filename
