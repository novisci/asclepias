-- |

module TestUtils.ConstructTestTree
  ( appTestCmdString
  , constructTestInputFragmFSS
  , constructTestOutputFragmFSS
  )where

import           TestUtils.TestCases
import           Test.Tasty                     ( TestTree )
import           Test.Tasty.Silver              ( goldenVsFile )
import           System.IO                      (FilePath)
import           System.Process                 ( callCommand )
import           TestUtils.TestCases            (TestScenarioCohort)

-- -- Conduct a single test
-- appGoldenVsFile :: String -> TestScenarioCohort -> TestTree
-- -- appGoldenVsFile :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> TestTree
-- appGoldenVsFile sessionId appType testDataType testInputType testOutputType =
--   goldenVsFile
--     (constructTestName appType testDataType testInputType testOutputType)
--     (createFilepathForGolden appType testDataType)
--     (createFilepathForResult appType testDataType testInputType testOutputType)
--     (appTest sessionId appType testDataType testInputType testOutputType)

appGoldenVsFile ::
     (TestScenarioCohort -> String)
  -> (TestScenarioCohort -> FilePath)
  -> (TestScenarioCohort -> FilePath)
  -> (TestScenarioCohort -> FilePath)
  -> (TestScenarioCohort -> IO ())
  -> TestScenarioCohort
  -> TestTree
appGoldenVsFile
  constructTestName
  constructFilepathForTest
  constructFilepathForGolden
  constructFilepathForResult
  appTest
  testScenario =
    goldenVsFile
      (constructTestName testScenario)
      (constructFilepathForGolden testScenario)
      (constructFilepathForResult testScenario)
      (appTest testScenario)

appTest ::
     (TestScenarioCohort -> IO ())
  -> (TestScenarioCohort -> IO ())
  -> (TestScenarioCohort -> IO ())
  -> TestScenarioCohort
  -> IO ()
appTest preCmdHook appTestCmd postCmdHook testScenario =
  do
    preCmdHook testScenario
    appTestCmd testScenario
    postCmdHook testScenario

appTestCmd ::
     (TestScenarioCohort -> String)
  -> TestScenarioCohort
  -> IO ()
appTestCmd appTestCmdString testScenario =
  do
    print $ "TEST COMMAND:  " ++ cmd
    pure cmd >>= callCommand
    where
      cmd = appTestCmdString testScenario

appTestCmdString ::
     (a -> String)
  -> (a -> String)
  -> (a -> String)
  -> a
  -> String
appTestCmdString
  constructTestExecutableFragm
  constructTestInputFragm
  constructTestOutputFragm
  testScenario =
    testExecutableFragm ++ " " ++ testInputFragm ++ " " ++ testOutputFragm
    where
      testExecutableFragm = constructTestExecutableFragm testScenario
      testInputFragm = constructTestInputFragm testScenario
      testOutputFragm = constructTestOutputFragm testScenario

constructTestInputFragmFSS ::
  (InputTypeAbleFSS a)
  => (a -> FilePath)
  -> (a -> String)
  -> (a -> String)
  -> a
  -> String
constructTestInputFragmFSS
  constructFilepathForTest
  constructBucketForTest
  constructS3KeyForTest
  testScenario =
    case extractTestInputType testScenario of
          TestInputFile -> "-f " ++ filepathForTest
          TestInputStdin -> "< " ++ filepathForTest
          TestInputS3 -> "-r us-east-1 -b " ++ bucket ++ " -k " ++ s3KeyForTest
    where
      filepathForTest = constructFilepathForTest testScenario
      bucket = constructBucketForTest testScenario
      s3KeyForTest = constructS3KeyForTest testScenario

constructTestOutputFragmFSS ::
     (TestScenarioCohort -> FilePath)
  -> (TestScenarioCohort -> String)
  -> (TestScenarioCohort -> String)
  -> TestScenarioCohort
  -> String
constructTestOutputFragmFSS
  constructFilepathForResult
  constructBucketForResult
  constructS3KeyForResult
  testScenario =
    case getCohortTestOutputType testScenario of
        TestOutputFile -> "-o " ++ filepathForResult
        TestOutputStdout -> "> " ++ filepathForResult
        TestOutputS3 -> "--outregion us-east-1 --outbucket " ++ bucket ++ " --outkey " ++ s3KeyForResult
    where
      filepathForResult = constructFilepathForResult testScenario
      bucket = constructBucketForResult testScenario
      s3KeyForResult = constructS3KeyForResult testScenario


-- -- Build a shell command represented by string and run the command as a
-- -- subprocess, where the command is a cohort-building application. If the
-- -- application writes the results to S3 then copy those results back to the
-- -- local filesystem
-- appTest :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> IO ()
-- appTest sessionId appType testDataType testInputType testOutputType = do
--   let outFilename = createFilenameForResult appType testDataType testInputType testOutputType
--   let isS3out = case testOutputType of
--         TestOutputS3 -> True
--         _ -> False
--   let cmd = appTestCmd sessionId appType testDataType testInputType testOutputType
--   print $ "TEST COMMAND:  " ++ cmd
--   pure cmd >>= callCommand
--   when isS3out $
--     s3Copy
--       (convNameToS3UriResult sessionId outFilename)
--       (convNameToPathResult outFilename)

-- -- Note that if the input is specified as coming from standard input, then a
-- -- fragment like `"< /path/to/file"` is inserted in the middle of the command
-- -- string. While this is not usual practice, the shell removes the fragment
-- -- prior to processing and things do indeed work as intended
-- appTestCmdBuilder ::
--      (TestScenarioCohort -> FilePath)
--   ->
-- appTestCmd sessionId id testInputType =
--   "exampleFilterApp " ++ inputFragm ++ " > " ++ outfilename
--   where
--     inputFragm = case testInputType of
--       TestInputFile -> "-f " ++ createLocalFilepathForTest id
--       TestInputStdin -> "< " ++ createLocalFilepathForTest id
--       TestInputS3 -> "-r us-east-1 -b " ++ s3Bucket ++ " -k " ++ createS3keyForTest sessionId id
--     outfilename = createLocalFilepathForResults id testInputType

-- -- Construct a string representing a shell command that runs one of the testing
-- -- cohort-building applications on the test data
-- --
-- -- Note that if the input is specified as coming from standard input, then a
-- -- fragment like `"< /path/to/file"` is inserted in the middle of the command
-- -- string. While this is not usual practice, the shell removes the fragment
-- -- prior to processing and things do indeed work as intended
-- appTestCmdString ::
--      (TestScenarioCohort -> FilePath)
--   -> (TestScenarioCohort -> FilePath)
--   -> (TestScenarioCohort -> FilePath)
--   -> (TestScenarioCohort -> FilePath)
--   -> TestScenarioCohort
--   -> String
-- appTestCmdString
--   constructFilepathForTest
--   constructFilepathForResult
--   constructS3UriForTest
--   constructS3UriForResult
--   testScenario =
--     appCmd ++ " " ++ inputFragm ++ " " ++ outputFragm
--     where
--       filePathForTest = constructFilepathForTest testScenario
--       filePathForResult = constructFilepathForTest testScenario
--       s3UriForTest = constructS3UriForTest testScenario
--       s3UriForResult = constructS3UriForResult testScenario
--       appCmd = case appType of
--           AppRowWise -> "exampleAppRW"
--           AppColumnWise -> "exampleAppCW"
--       inputFragm = case testInputType of
--           TestInputFile -> "-f " ++ convNameToPathTest inFilename
--           TestInputStdin -> "< " ++ convNameToPathTest inFilename
--           TestInputS3 -> "-r us-east-1 -b " ++ s3Bucket ++ " -k " ++ convNameToS3KeyTest sessionId inFilename
--       outputFragm = case testOutputType of
--           TestOutputFile -> "-o " ++ convNameToPathResult outFilename
--           TestOutputStdout -> "> " ++ convNameToPathResult outFilename
--           TestOutputS3 -> "--outregion us-east-1 --outbucket " ++ s3Bucket ++ " --outkey " ++ convNameToS3KeyResult sessionId outFilename
