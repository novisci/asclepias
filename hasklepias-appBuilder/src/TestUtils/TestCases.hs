-- |

module TestUtils.TestCases
  ( AppType(..)
  , TestDataType(..)
  , TestInputType(..)
  , TestOutputType(..)
  , createTestsCartesian
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

--
data TestScenario = TestScenario
  { getAppType :: AppType
  , getTestDataType :: TestDataType
  , getTestInputType :: TestInputType
  , getTestOutputType :: TestOutputType
  }

-- Convenience synonym for a TestTree constructor
type TestElem = AppType -> TestDataType -> TestInputType -> TestOutputType -> TestTree

-- Enumerate the test cases
createTestsCartesian :: TestElem -> TestTree
createTestsCartesian f = testGroup
  "Tests of exampleApp"
  [ f AppRowWise    TestDataEmpty     TestInputFile  TestOutputFile
  , f AppRowWise    TestDataEmpty     TestInputFile  TestOutputStdout
  , f AppRowWise    TestDataEmpty     TestInputFile  TestOutputS3
  , f AppRowWise    TestDataEmpty     TestInputStdin TestOutputFile
  , f AppRowWise    TestDataEmpty     TestInputStdin TestOutputStdout
  , f AppRowWise    TestDataEmpty     TestInputStdin TestOutputS3
  , f AppRowWise    TestDataEmpty     TestInputS3    TestOutputFile
  , f AppRowWise    TestDataEmpty     TestInputS3    TestOutputStdout
  , f AppRowWise    TestDataEmpty     TestInputS3    TestOutputS3
  , f AppRowWise    TestDataSmall     TestInputFile  TestOutputFile
  , f AppRowWise    TestDataSmall     TestInputFile  TestOutputStdout
  , f AppRowWise    TestDataSmall     TestInputFile  TestOutputS3
  , f AppRowWise    TestDataSmall     TestInputStdin TestOutputFile
  , f AppRowWise    TestDataSmall     TestInputStdin TestOutputStdout
  , f AppRowWise    TestDataSmall     TestInputStdin TestOutputS3
  , f AppRowWise    TestDataSmall     TestInputS3    TestOutputFile
  , f AppRowWise    TestDataSmall     TestInputS3    TestOutputStdout
  , f AppRowWise    TestDataSmall     TestInputS3    TestOutputS3
  , f AppRowWise    TestDataManySubj  TestInputFile  TestOutputFile
  , f AppRowWise    TestDataManySubj  TestInputFile  TestOutputStdout
  , f AppRowWise    TestDataManySubj  TestInputFile  TestOutputS3
  , f AppRowWise    TestDataManySubj  TestInputStdin TestOutputFile
  , f AppRowWise    TestDataManySubj  TestInputStdin TestOutputStdout
  , f AppRowWise    TestDataManySubj  TestInputStdin TestOutputS3
  , f AppRowWise    TestDataManySubj  TestInputS3    TestOutputFile
  , f AppRowWise    TestDataManySubj  TestInputS3    TestOutputStdout
  , f AppRowWise    TestDataManySubj  TestInputS3    TestOutputS3
  , f AppRowWise    TestDataManyEvent TestInputFile  TestOutputFile
  , f AppRowWise    TestDataManyEvent TestInputFile  TestOutputStdout
  , f AppRowWise    TestDataManyEvent TestInputFile  TestOutputS3
  , f AppRowWise    TestDataManyEvent TestInputStdin TestOutputFile
  , f AppRowWise    TestDataManyEvent TestInputStdin TestOutputStdout
  , f AppRowWise    TestDataManyEvent TestInputStdin TestOutputS3
  , f AppRowWise    TestDataManyEvent TestInputS3    TestOutputFile
  , f AppRowWise    TestDataManyEvent TestInputS3    TestOutputStdout
  , f AppRowWise    TestDataManyEvent TestInputS3    TestOutputS3
  , f AppColumnWise TestDataEmpty     TestInputFile  TestOutputFile
  , f AppColumnWise TestDataEmpty     TestInputFile  TestOutputStdout
  , f AppColumnWise TestDataEmpty     TestInputFile  TestOutputS3
  , f AppColumnWise TestDataEmpty     TestInputStdin TestOutputFile
  , f AppColumnWise TestDataEmpty     TestInputStdin TestOutputStdout
  , f AppColumnWise TestDataEmpty     TestInputStdin TestOutputS3
  , f AppColumnWise TestDataEmpty     TestInputS3    TestOutputFile
  , f AppColumnWise TestDataEmpty     TestInputS3    TestOutputStdout
  , f AppColumnWise TestDataEmpty     TestInputS3    TestOutputS3
  , f AppColumnWise TestDataSmall     TestInputFile  TestOutputFile
  , f AppColumnWise TestDataSmall     TestInputFile  TestOutputStdout
  , f AppColumnWise TestDataSmall     TestInputFile  TestOutputS3
  , f AppColumnWise TestDataSmall     TestInputStdin TestOutputFile
  , f AppColumnWise TestDataSmall     TestInputStdin TestOutputStdout
  , f AppColumnWise TestDataSmall     TestInputStdin TestOutputS3
  , f AppColumnWise TestDataSmall     TestInputS3    TestOutputFile
  , f AppColumnWise TestDataSmall     TestInputS3    TestOutputStdout
  , f AppColumnWise TestDataSmall     TestInputS3    TestOutputS3
  , f AppColumnWise TestDataManySubj  TestInputFile  TestOutputFile
  , f AppColumnWise TestDataManySubj  TestInputFile  TestOutputStdout
  , f AppColumnWise TestDataManySubj  TestInputFile  TestOutputS3
  , f AppColumnWise TestDataManySubj  TestInputStdin TestOutputFile
  , f AppColumnWise TestDataManySubj  TestInputStdin TestOutputStdout
  , f AppColumnWise TestDataManySubj  TestInputStdin TestOutputS3
  , f AppColumnWise TestDataManySubj  TestInputS3    TestOutputFile
  , f AppColumnWise TestDataManySubj  TestInputS3    TestOutputStdout
  , f AppColumnWise TestDataManySubj  TestInputS3    TestOutputS3
  , f AppColumnWise TestDataManyEvent TestInputFile  TestOutputFile
  , f AppColumnWise TestDataManyEvent TestInputFile  TestOutputStdout
  , f AppColumnWise TestDataManyEvent TestInputFile  TestOutputS3
  , f AppColumnWise TestDataManyEvent TestInputStdin TestOutputFile
  , f AppColumnWise TestDataManyEvent TestInputStdin TestOutputStdout
  , f AppColumnWise TestDataManyEvent TestInputStdin TestOutputS3
  , f AppColumnWise TestDataManyEvent TestInputS3    TestOutputFile
  , f AppColumnWise TestDataManyEvent TestInputS3    TestOutputStdout
  , f AppColumnWise TestDataManyEvent TestInputS3    TestOutputS3
  ]
