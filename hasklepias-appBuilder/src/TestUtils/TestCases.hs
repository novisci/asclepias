-- |

module TestUtils.TestCases
  ( AppType(..)
  , InputTypeAbleFSS(..)
  , TestCollectorInputType(..)
  , TestCollectorOutputType(..)
  , TestCollectorScenario(..)
  , TestDataType(..)
  , TestInputType(..)
  , TestOutputType(..)
  , TestScenarioCohort(..)
  , createCollectorTests
  , createTestsCartesian
  ) where

import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           TestUtils.S3Utils

-- Enumeration of test applications
data AppType = AppRowWise | AppColumnWise

-- Enumeration of test data cases
data TestDataType = TestDataEmpty | TestDataSmall | TestDataManySubj | TestDataManyEvent

-- Enumeration of input sources
data TestInputType = TestInputFile | TestInputStdin | TestInputS3

-- Enumeration of output sources
data TestOutputType = TestOutputFile | TestOutputStdout | TestOutputS3

-- Enumeration of input sources for cohort-collector
--
-- TODO: there are other input sources not covered here: --dir and --bucket
data TestCollectorInputType = TestCollectorInputFile | TestCollectorInputS3

-- Enumeration of output sources for cohort-collector
--
-- TODO: there are other output sources not covered here: --outdir
data TestCollectorOutputType =
    TestCollectorOutputFile
  | TestCollectorOutputStdout
  | TestCollectorOutputS3

-- Test scenario type for the filter application
data TestScenarioFilter = TestScenarioFilter
  { getFilterTestDataType :: TestDataType
  , getFilterTestInputType :: TestInputType
  }

-- Test scenario type for the cohort application
data TestScenarioCohort = TestScenarioCohort
  { getCohortAppType :: AppType
  , getCohortTestDataType :: TestDataType
  , getCohortTestInputType :: TestInputType
  , getCohortTestOutputType :: TestOutputType
  }

-- Test scenario type for the cohort-collector application
data TestCollectorScenario = TestCollectorScenario
  { getTestCollectorAppType :: AppType
  , getTestCollectorInputType :: TestCollectorInputType
  , getTestCollectorOutputType :: TestCollectorOutputType
  }

-- class InputFragmAble a where
--   constructInputFragm ::
--        (a -> FilePath)
--     -> (a -> String)
--     -> (a -> String)
--     -> a
--     -> String

class InputTypeAbleFSS a where
  extractTestInputType :: a -> TestInputType

instance InputTypeAbleFSS TestScenarioCohort where
  extractTestInputType = getCohortTestInputType

-- class OutputTypeAbleFSS a where
--   extractTestOutputType :: a -> TestOutputType

-- instance OutputTypeAbleFSS TestScenarioCohort where
--   extractTestOutputType = getCohortTestOutputType

-- instance OutputTypeAbleFSS TestCollectorScenario where
--   extractTestOutputType = getTestCollectorOutputType

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


-- preCmdHook :: TestScenarioCohort -> IO ()
-- preCmdHook _ = pure ()

-- Enumerate the test cases
createTestsCartesian :: String -> (TestScenarioCohort -> TestTree) -> TestTree
createTestsCartesian testName appTest = testGroup
  testName
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
    appTestWrp appType testDataType testInputType testOutputType =
      appTest (TestScenarioCohort appType testDataType testInputType testOutputType)

createCollectorTests :: String -> (TestCollectorScenario -> TestTree) -> TestTree
createCollectorTests testName appTest = testGroup
  testName
  [ appTestWrp AppRowWise    TestCollectorInputFile TestCollectorOutputFile
  , appTestWrp AppRowWise    TestCollectorInputFile TestCollectorOutputStdout
  , appTestWrp AppRowWise    TestCollectorInputFile TestCollectorOutputS3
  , appTestWrp AppRowWise    TestCollectorInputS3   TestCollectorOutputFile
  , appTestWrp AppRowWise    TestCollectorInputS3   TestCollectorOutputStdout
  , appTestWrp AppRowWise    TestCollectorInputS3   TestCollectorOutputS3
  , appTestWrp AppColumnWise TestCollectorInputFile TestCollectorOutputFile
  , appTestWrp AppColumnWise TestCollectorInputFile TestCollectorOutputStdout
  , appTestWrp AppColumnWise TestCollectorInputFile TestCollectorOutputS3
  , appTestWrp AppColumnWise TestCollectorInputS3   TestCollectorOutputFile
  , appTestWrp AppColumnWise TestCollectorInputS3   TestCollectorOutputStdout
  , appTestWrp AppColumnWise TestCollectorInputS3   TestCollectorOutputS3
  ]
  where
    appTestWrp appType inputType outputType =
      appTest (TestCollectorScenario appType inputType outputType)
