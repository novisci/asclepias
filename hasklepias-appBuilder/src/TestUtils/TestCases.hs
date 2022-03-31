-- |

module TestUtils.TestCases
  ( AppType(..)
  , TestDataType(..)
  , TestInputType(..)
  , TestOutputType(..)
  , TestScenarioCohort(..)
  -- , createTestsCartesian
  ) where

import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )

-- Enumeration of the test applications
data AppType = AppRowWise | AppColumnWise

-- Enumeration of the test data cases
data TestDataType = TestDataEmpty | TestDataSmall | TestDataManySubj | TestDataManyEvent

-- Enumeration of input sources
data TestInputType = TestInputFile | TestInputStdin | TestInputS3

-- Enumeration of output sources
data TestOutputType = TestOutputFile | TestOutputStdout | TestOutputS3

-- Product type of the various scenarios
data TestScenarioCohort = TestScenarioCohort
  { getCohortAppType :: AppType
  , getCohortTestDataType :: TestDataType
  , getCohortTestInputType :: TestInputType
  , getCohortTestOutputType :: TestOutputType
  }

-- class InputFragmAble a where
--   constructInputFragm ::
--        (a -> FilePath)
--     -> (a -> String)
--     -> (a -> String)
--     -> a
--     -> String

class InputTypeAble a where
  extractTestInputType :: a -> TestInputType

instance InputTypeAble TestScenarioCohort where
  extractTestInputType = getCohortTestInputType

-- instance InputFragmAble TestScenarioCohort where
--   constructInputFragm testScenarioCohort =


-- -- Product type of the various scenarios
-- data TestScenarioCohort' = TestScenarioCohort'
--   { getAppType :: AppType
--   , getTestDataType :: TestDataType
--   , getTestInputType :: TestInputType
--   , getTestOutputType :: TestOutputType
--   }

-- -- Convenience synonym for a TestTree constructor
-- type TestElem = AppType -> TestDataType -> TestInputType -> TestOutputType -> TestTree

constructTestInputFragm ::
  (InputTypeAble a)
  => (a -> FilePath)
  -> (a -> String)
  -> (a -> String)
  -> a
  -> String
constructTestInputFragm
  constructFilePathForTest
  constructBucketForTest
  constructS3KeyForTest
  testScenario =
    case extractTestInputType testScenario of
          TestInputFile -> "-f " ++ filePathForTest
          TestInputStdin -> "< " ++ filePathForTest
          TestInputS3 -> "-r us-east-1 -b " ++ bucket ++ " -k " ++ s3KeyForTest
    where
      filePathForTest = constructFilePathForTest testScenario
      bucket = constructBucketForTest testScenario
      s3KeyForTest = constructS3KeyForTest testScenario

constructTestOutputFragm ::
     (TestScenarioCohort -> FilePath)
  -> (TestScenarioCohort -> String)
  -> (TestScenarioCohort -> String)
  -> TestScenarioCohort
  -> String
constructTestOutputFragm
  constructFilePathForResult
  constructBucketForResult
  constructS3KeyForResult
  testScenario =
    case getCohortTestOutputType testScenario of
        TestOutputFile -> "-o " ++ filePathForResult
        TestOutputStdout -> "> " ++ filePathForResult
        TestOutputS3 -> "--outregion us-east-1 --outbucket " ++ bucket ++ " --outkey " ++ s3KeyForResult
    where
      filePathForResult = constructFilePathForResult testScenario
      bucket = constructBucketForResult testScenario
      s3KeyForResult = constructS3KeyForResult testScenario

-- preCmdHook :: TestScenarioCohort -> IO ()
-- preCmdHook _ = pure ()

-- -- postCmdHook :: TestScenarioCohort -> IO ()
-- postCmdHook ::
--      (TestScenarioCohort -> String)
--   -> (TestScenarioCohort -> FilePath)
--   -> TestScenarioCohort
--   -> IO ()
-- postCmdHook constructS3UriForResult constructFilePathForResult testScenario =
--   do
--     let a = 22
--     pure ()
--     -- let isS3out = case testOutputType of
--     --   ITestOutputS3 -> True
--     --   _ -> False
--     -- when isS3out $
--     --   s3Copy
--     --     (constructS3UriForResult testScenario)
--     --     (convNameToPathResult testScenario)

-- Enumerate the test cases
createTestsCartesian :: String -> (TestScenarioCohort -> TestTree) -> TestTree
createTestsCartesian name appTest = testGroup
  "Tests of exampleApp"
  [ appTestWrp AppRowWise    TestDataEmpty     TestInputFile  TestOutputFile
  , appTestWrp AppRowWise    TestDataEmpty     TestInputFile  TestOutputStdout
  , appTestWrp AppRowWise    TestDataEmpty     TestInputFile  TestOutputS3
  , appTestWrp AppRowWise    TestDataEmpty     TestInputStdin TestOutputFile
  , appTestWrp AppRowWise    TestDataEmpty     TestInputStdin TestOutputStdout
  , appTestWrp AppRowWise    TestDataEmpty     TestInputStdin TestOutputS3
  , appTestWrp AppRowWise    TestDataEmpty     TestInputS3    TestOutputFile
  , appTestWrp AppRowWise    TestDataEmpty     TestInputS3    TestOutputStdout
  , appTestWrp AppRowWise    TestDataEmpty     TestInputS3    TestOutputS3
  , appTestWrp AppRowWise    TestDataSmall     TestInputFile  TestOutputFile
  , appTestWrp AppRowWise    TestDataSmall     TestInputFile  TestOutputStdout
  , appTestWrp AppRowWise    TestDataSmall     TestInputFile  TestOutputS3
  , appTestWrp AppRowWise    TestDataSmall     TestInputStdin TestOutputFile
  , appTestWrp AppRowWise    TestDataSmall     TestInputStdin TestOutputStdout
  , appTestWrp AppRowWise    TestDataSmall     TestInputStdin TestOutputS3
  , appTestWrp AppRowWise    TestDataSmall     TestInputS3    TestOutputFile
  , appTestWrp AppRowWise    TestDataSmall     TestInputS3    TestOutputStdout
  , appTestWrp AppRowWise    TestDataSmall     TestInputS3    TestOutputS3
  , appTestWrp AppRowWise    TestDataManySubj  TestInputFile  TestOutputFile
  , appTestWrp AppRowWise    TestDataManySubj  TestInputFile  TestOutputStdout
  , appTestWrp AppRowWise    TestDataManySubj  TestInputFile  TestOutputS3
  , appTestWrp AppRowWise    TestDataManySubj  TestInputStdin TestOutputFile
  , appTestWrp AppRowWise    TestDataManySubj  TestInputStdin TestOutputStdout
  , appTestWrp AppRowWise    TestDataManySubj  TestInputStdin TestOutputS3
  , appTestWrp AppRowWise    TestDataManySubj  TestInputS3    TestOutputFile
  , appTestWrp AppRowWise    TestDataManySubj  TestInputS3    TestOutputStdout
  , appTestWrp AppRowWise    TestDataManySubj  TestInputS3    TestOutputS3
  , appTestWrp AppRowWise    TestDataManyEvent TestInputFile  TestOutputFile
  , appTestWrp AppRowWise    TestDataManyEvent TestInputFile  TestOutputStdout
  , appTestWrp AppRowWise    TestDataManyEvent TestInputFile  TestOutputS3
  , appTestWrp AppRowWise    TestDataManyEvent TestInputStdin TestOutputFile
  , appTestWrp AppRowWise    TestDataManyEvent TestInputStdin TestOutputStdout
  , appTestWrp AppRowWise    TestDataManyEvent TestInputStdin TestOutputS3
  , appTestWrp AppRowWise    TestDataManyEvent TestInputS3    TestOutputFile
  , appTestWrp AppRowWise    TestDataManyEvent TestInputS3    TestOutputStdout
  , appTestWrp AppRowWise    TestDataManyEvent TestInputS3    TestOutputS3
  , appTestWrp AppColumnWise TestDataEmpty     TestInputFile  TestOutputFile
  , appTestWrp AppColumnWise TestDataEmpty     TestInputFile  TestOutputStdout
  , appTestWrp AppColumnWise TestDataEmpty     TestInputFile  TestOutputS3
  , appTestWrp AppColumnWise TestDataEmpty     TestInputStdin TestOutputFile
  , appTestWrp AppColumnWise TestDataEmpty     TestInputStdin TestOutputStdout
  , appTestWrp AppColumnWise TestDataEmpty     TestInputStdin TestOutputS3
  , appTestWrp AppColumnWise TestDataEmpty     TestInputS3    TestOutputFile
  , appTestWrp AppColumnWise TestDataEmpty     TestInputS3    TestOutputStdout
  , appTestWrp AppColumnWise TestDataEmpty     TestInputS3    TestOutputS3
  , appTestWrp AppColumnWise TestDataSmall     TestInputFile  TestOutputFile
  , appTestWrp AppColumnWise TestDataSmall     TestInputFile  TestOutputStdout
  , appTestWrp AppColumnWise TestDataSmall     TestInputFile  TestOutputS3
  , appTestWrp AppColumnWise TestDataSmall     TestInputStdin TestOutputFile
  , appTestWrp AppColumnWise TestDataSmall     TestInputStdin TestOutputStdout
  , appTestWrp AppColumnWise TestDataSmall     TestInputStdin TestOutputS3
  , appTestWrp AppColumnWise TestDataSmall     TestInputS3    TestOutputFile
  , appTestWrp AppColumnWise TestDataSmall     TestInputS3    TestOutputStdout
  , appTestWrp AppColumnWise TestDataSmall     TestInputS3    TestOutputS3
  , appTestWrp AppColumnWise TestDataManySubj  TestInputFile  TestOutputFile
  , appTestWrp AppColumnWise TestDataManySubj  TestInputFile  TestOutputStdout
  , appTestWrp AppColumnWise TestDataManySubj  TestInputFile  TestOutputS3
  , appTestWrp AppColumnWise TestDataManySubj  TestInputStdin TestOutputFile
  , appTestWrp AppColumnWise TestDataManySubj  TestInputStdin TestOutputStdout
  , appTestWrp AppColumnWise TestDataManySubj  TestInputStdin TestOutputS3
  , appTestWrp AppColumnWise TestDataManySubj  TestInputS3    TestOutputFile
  , appTestWrp AppColumnWise TestDataManySubj  TestInputS3    TestOutputStdout
  , appTestWrp AppColumnWise TestDataManySubj  TestInputS3    TestOutputS3
  , appTestWrp AppColumnWise TestDataManyEvent TestInputFile  TestOutputFile
  , appTestWrp AppColumnWise TestDataManyEvent TestInputFile  TestOutputStdout
  , appTestWrp AppColumnWise TestDataManyEvent TestInputFile  TestOutputS3
  , appTestWrp AppColumnWise TestDataManyEvent TestInputStdin TestOutputFile
  , appTestWrp AppColumnWise TestDataManyEvent TestInputStdin TestOutputStdout
  , appTestWrp AppColumnWise TestDataManyEvent TestInputStdin TestOutputS3
  , appTestWrp AppColumnWise TestDataManyEvent TestInputS3    TestOutputFile
  , appTestWrp AppColumnWise TestDataManyEvent TestInputS3    TestOutputStdout
  , appTestWrp AppColumnWise TestDataManyEvent TestInputS3    TestOutputS3
  ]
  where
    appTestWrp appRowWise testDataEmpty testInputFile testOutputFile =
      appTest (TestScenarioCohort appRowWise testDataEmpty testInputFile testOutputFile)
