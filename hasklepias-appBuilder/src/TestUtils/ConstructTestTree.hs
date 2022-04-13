module TestUtils.ConstructTestTree
  ( appGoldenVsFile
  , appTest
  , appTestCmd
  , appTestCmdString
  , constructTestCollectorInputFragm
  , constructTestCollectorOutputFragm
  , constructTestInputFragmFSS
  , constructTestOutputFragmFSS
  , postCollectorCmdHookS3
  , postCmdHookS3
  ) where

import           Amazonka.S3.HeadObject         ( headObjectResponse_replicationStatus
                                                )
import           Control.Monad                  ( when )
import           System.IO                      ( FilePath )
import           System.Process                 ( callCommand )
import           Test.Tasty                     ( TestTree )
import           Test.Tasty.Silver              ( goldenVsFile )
import           TestUtils.S3Utils
import           TestUtils.TestCases

-- Conduct a single test
appGoldenVsFile
  :: (a -> String)
  -> (a -> FilePath)
  -> (a -> FilePath)
  -> (a -> FilePath)
  -> (a -> IO ())
  -> a
  -> TestTree
appGoldenVsFile constructTestName constructFilepathForTest constructFilepathForGolden constructFilepathForResult appTest testScenario
  = goldenVsFile (constructTestName testScenario)
                 (constructFilepathForGolden testScenario)
                 (constructFilepathForResult testScenario)
                 (appTest testScenario)

-- Perform a pre-command action, perform the command that the test will be
-- assessing, and then perform a post-command action
appTest :: (a -> IO ()) -> (a -> IO ()) -> (a -> IO ()) -> a -> IO ()
appTest preCmdHook appTestCmd postCmdHook testScenario = do
  preCmdHook testScenario
  appTestCmd testScenario
  postCmdHook testScenario

-- Perform the command that the test will be assessing
appTestCmd :: (a -> String) -> a -> IO ()
appTestCmd appTestCmdString testScenario = do
  print $ "TEST COMMAND:  " ++ cmd
  pure cmd >>= callCommand
  where cmd = appTestCmdString testScenario

-- Construct a shell string that can be run as a command. The string is
-- constructed in three string fragments: the excecutable command, a fragment
-- specifying where the command gets its input, and a fragment specifying where
-- the command gets its output
appTestCmdString
  :: (a -> String) -> (a -> String) -> (a -> String) -> a -> String
appTestCmdString constructTestExecutableFragm constructTestInputFragm constructTestOutputFragm testScenario
  = testExecutableFragm ++ " " ++ testInputFragm ++ " " ++ testOutputFragm
 where
  testExecutableFragm = constructTestExecutableFragm testScenario
  testInputFragm      = constructTestInputFragm testScenario
  testOutputFragm     = constructTestOutputFragm testScenario

-- Construct a fragment of a shell string specifying the input for a testing
-- scenario where the input can come from a file, standard input, or from Amazon
-- S3
--
-- Note that if the input is specified as coming from standard input, then a
-- fragment like `"< /path/to/file"` is inserted in the middle of the overall
-- command string. While this is not usual practice, the shell removes the
-- fragment prior to processing and things do indeed work as intended
constructTestInputFragmFSS
  :: (InputTypeAbleFSS a)
  => (a -> FilePath)
  -> (a -> String)
  -> (a -> String)
  -> a
  -> String
constructTestInputFragmFSS constructFilepathForTest constructBucketForTest constructS3KeyForTest testScenario
  = case extractTestInputType testScenario of
    TestInputFile  -> "-f " ++ filepathForTest
    TestInputStdin -> "< " ++ filepathForTest
    TestInputS3    -> "-r us-east-1 -b " ++ bucket ++ " -k " ++ s3KeyForTest
 where
  filepathForTest = constructFilepathForTest testScenario
  bucket          = constructBucketForTest testScenario
  s3KeyForTest    = constructS3KeyForTest testScenario

-- NOTE: this is nearly identical to the above function, but with different
-- input types (and `-m` option instead of `-k`). Can we combine the two
-- somehow?
constructTestCollectorInputFragm
  :: (TestCollectorScenario -> FilePath)
  -> (TestCollectorScenario -> String)
  -> (TestCollectorScenario -> String)
  -> TestCollectorScenario
  -> String
constructTestCollectorInputFragm constructFilepathForTest constructBucketForTest constructS3KeyForTest testCollectorScenario
  = case getTestCollectorInputType testCollectorScenario of
    TestCollectorInputFile -> "-f " ++ filepathForTest
    TestCollectorInputS3   -> "-b " ++ bucket ++ " -m " ++ s3KeyForTest
 where
  filepathForTest = constructFilepathForTest testCollectorScenario
  bucket          = constructBucketForTest testCollectorScenario
  s3KeyForTest    = constructS3KeyForTest testCollectorScenario

-- Construct a fragment of a shell string specifying the output for a testing
-- scenario where the output can be written to a file, standard output, or from
-- Amazon S3
constructTestOutputFragmFSS
  :: (TestScenarioCohort -> FilePath)
  -> (TestScenarioCohort -> String)
  -> (TestScenarioCohort -> String)
  -> TestScenarioCohort
  -> String
constructTestOutputFragmFSS constructFilepathForResult constructBucketForResult constructS3KeyForResult testScenario
  = case getCohortTestOutputType testScenario of
    TestOutputFile   -> "-o " ++ filepathForResult
    TestOutputStdout -> "> " ++ filepathForResult
    TestOutputS3 ->
      "--outregion us-east-1 --outbucket "
        ++ bucket
        ++ " --outkey "
        ++ s3KeyForResult
 where
  filepathForResult = constructFilepathForResult testScenario
  bucket            = constructBucketForResult testScenario
  s3KeyForResult    = constructS3KeyForResult testScenario

-- Construct a fragment of a shell string specifying the output for a testing
-- scenario for the cohort-collector application
--
-- NOTE: this is nearly identical to the above function, but with different
-- output types. Can we combine the two somehow?
constructTestCollectorOutputFragm
  :: (TestCollectorScenario -> FilePath)
  -> (TestCollectorScenario -> String)
  -> (TestCollectorScenario -> String)
  -> TestCollectorScenario
  -> String
constructTestCollectorOutputFragm constructFilepathForResult constructBucketForResult constructS3KeyForResult testScenario
  = case getTestCollectorOutputType testScenario of
    TestCollectorOutputFile   -> "-o " ++ filepathForResult
    TestCollectorOutputStdout -> "> " ++ filepathForResult
    TestCollectorOutputS3 ->
      "--outregion us-east-1 --outbucket "
        ++ bucket
        ++ " --outkey "
        ++ s3KeyForResult
 where
  filepathForResult = constructFilepathForResult testScenario
  bucket            = constructBucketForResult testScenario
  s3KeyForResult    = constructS3KeyForResult testScenario

-- Construct a post command hook that copies the output produced by the command
-- under test from S3 to the local filesystem
postCmdHookS3
  :: (TestScenarioCohort -> String)
  -> (TestScenarioCohort -> FilePath)
  -> TestScenarioCohort
  -> IO ()
postCmdHookS3 constructS3UriForResult constructFilepathForResult testScenario =
  when
    isS3out
    (s3Copy (constructS3UriForResult testScenario)
            (constructFilepathForResult testScenario)
    )
 where
  isS3out = case getCohortTestOutputType testScenario of
    TestOutputS3 -> True
    _            -> False

-- NOTE: this is very similar to `postCmdHookS3`, can they be combined?
postCollectorCmdHookS3
  :: (TestCollectorScenario -> String)
  -> (TestCollectorScenario -> FilePath)
  -> TestCollectorScenario
  -> IO ()
postCollectorCmdHookS3 constructS3UriForResult constructFilepathForResult testScenario
  = when
    isS3out
    (s3Copy (constructS3UriForResult testScenario)
            (constructFilepathForResult testScenario)
    )
 where
  isS3out = case getTestCollectorOutputType testScenario of
    TestCollectorOutputS3 -> True
    _                     -> False
