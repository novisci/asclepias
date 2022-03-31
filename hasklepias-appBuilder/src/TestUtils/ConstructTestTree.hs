-- |

module TestUtils.ConstructTestTree where

import           TestUtils.TestCases
import           Test.Tasty                     ( TestTree )
import           Test.Tasty.Silver              ( goldenVsFile )

-- Conduct a single test
appGoldenVsFile :: String -> TestScenario -> TestTree
-- appGoldenVsFile :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> TestTree
appGoldenVsFile sessionId appType testDataType testInputType testOutputType =
  goldenVsFile
    (constructTestName appType testDataType testInputType testOutputType)
    (createFilepathForGolden appType testDataType)
    (createFilepathForResult appType testDataType testInputType testOutputType)
    (appTest sessionId appType testDataType testInputType testOutputType)

-- Build a shell command represented by string and run the command as a
-- subprocess, where the command is a cohort-building application. If the
-- application writes the results to S3 then copy those results back to the
-- local filesystem
appTest :: String -> AppType -> TestDataType -> TestInputType -> TestOutputType -> IO ()
appTest sessionId appType testDataType testInputType testOutputType = do
  let outFilename = createFilenameForResult appType testDataType testInputType testOutputType
  let isS3out = case testOutputType of
        TestOutputS3 -> True
        _ -> False
  let cmd = appTestCmd sessionId appType testDataType testInputType testOutputType
  print $ "TEST COMMAND:  " ++ cmd
  pure cmd >>= callCommand
  when isS3out $
    s3Copy
      (convNameToS3UriResult sessionId outFilename)
      (convNameToPathResult outFilename)
