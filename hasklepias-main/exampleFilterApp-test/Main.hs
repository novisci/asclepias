{-| Tests of the example filter application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Concurrent
import qualified Data.ByteString.Char8         as B
import           Hasklepias
import           Hasklepias.ExampleFilterApp
import           System.IO
import           System.IO.Silently
import           System.Process                 ( callCommand )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Silver

{-
Test case file names follow this convention:

test-n-m-v.jsonl
     ^ ^ ^
     | | +--- version (e.g. for different numbers of events or permutations of the order of subject and/or their events)
     | +----- number of subject that do NOT have any event satisfying predicate
     +------- number of subject that *do* have some event satisfying predicate

-}

localTestDataDir :: String
localTestDataDir = "exampleFilterApp-test/test/"

localResultsDir :: String
localResultsDir = "exampleFilterApp-test/results/"

s3Bucket :: String
s3Bucket = "download.novisci.com"

s3TestDataDir :: String
s3TestDataDir = "hasklepias/sandbox-testapps/testdata/"

largeInputSize :: Int
largeInputSize = 1000

-- Enumeration of input sources
data TestInputType = TestInputFile | TestInputStdin | TestInputS3

main :: IO ()
main = defaultMain tests

runTest :: String -> IO ()
runTest x = do
    -- The filter application writes to stdout as it goes, hence I use the 
    -- capture function to collect that output. Unfortunately, this has the 
    -- effect of adding some non-determinism to the test suite because it can 
    -- happen that the output of the test module itself is also captured.
    -- Hence, I'm adding some stdout flushing and delays to try to prevent that.
    -- FWIW, the filter app could probably be architected such that capture is
    -- not needed, but that requires more willpower and knowledge than I have 
    -- at the moment. -- bsaul 2021-10-27
  hFlush stdout -- flush stdout as test info sometimes being sent to file if running test
  threadDelay 500 -- pause 500 microseconds
  r <- capture $ runFilterAppWithLocation
    (Local (localTestDataDir <> "test-" <> x <> ".jsonl"))
    exampleFilterApp

  hFlush stdout -- flush stdout as test info sometimes being sent to file if running tests
  B.writeFile (localTestDataDir <> "test-" <> x <> ".result") (B.pack (fst r) <> snd r)


makeTest :: TestName -> String -> TestTree
makeTest n x = goldenVsFile n
                            (localTestDataDir <> "test-" <> x <> ".golden")
                            (localTestDataDir <> "test-" <> x <> ".result")
                            (runTest x)

makeTests :: [String] -> [TestTree]
makeTests = fmap (\x -> makeTest ("Test case " <> x) x)

tests :: TestTree
tests = testGroup
  "Tests of exampleFilterApp"
  (makeTests testIds)

testIds =
  [ "0-0-1"
  , "0-1-1"
  , "0-1-2"
  , "0-2-1"
  , "0-3-1"
  , "1-0-1"
  , "1-0-2"
  , "1-0-3"
  , "1-0-4"
  , "1-0-5"
  , "1-1-1"
  , "1-1-2"
  , "1-1-3"
  , "1-1-4"
  , "1-2-1"
  , "1-2-2"
  , "1-2-3"
  , "2-0-1"
  , "2-0-2"
  , "2-0-3"
  , "2-0-4"
  , "2-1-1"
  , "2-1-2"
  , "2-1-3"
  , "2-1-4"
  , "3-0-1"
  , "3-0-2"
  , "3-0-3"
  , "3-0-4"
  -- , largeInputSize ++ "-" ++ largeInputSize ++ "-1"
  -- , largeInputSize ++ "-" ++ largeInputSize ++ "-2"
  -- , largeInputSize ++ "-" ++ largeInputSize ++ "-3"
  -- , largeInputSize ++ "-" ++ largeInputSize ++ "-4"
  ]

-- Conduct a single test
appGoldenVsFile :: String -> String -> TestInputType -> TestTree
appGoldenVsFile sessionId testId testInputType =
  goldenVsFile
    ("Test case " ++ testId ++ " reading from " ++ inputStr)
    (localGoldenFilepath testId)
    (localResultsFilepath (resultsFilename testId testInputType))
    (appTest sessionId testId testInputType)
    where
      inputStr = case testInputType of
        TestInputFile -> "file"
        TestInputStdin -> "standard input"
        TestInputS3 -> "S3"

-- Build a shell command represented by string and run the command as a
-- subprocess, where the command is a cohort-building application
appTest :: String -> String -> TestInputType -> IO ()
appTest sessionId testId testInputType = do
  let outfilename = resultsFilename testId testInputType
  let cmd = appTestCmd sessionId testId testInputType
  pure cmd >>= callCommand

-- Construct a string representing a shell command that runs one of the testing
-- cohort-building applications on the test data
--
-- Note that if the input is specified as coming from standard input, then a
-- fragment like `"< /path/to/file"` is inserted in the middle of the command
-- string. While this is not usual practice, the shell removes the fragment
-- prior to processing and things do indeed work as intended
appTestCmd :: String -> String -> TestInputType -> String
appTestCmd sessionId testId testInputType =
  "exampleFilterApp " ++ inputFragm ++ " > " ++ outfilename
  where
    infilename = inputDataFilename testId
    outfilename = resultsFilename testId testInputType
    inputFragm = case testInputType of
      TestInputFile -> "-f " ++ infilename
      TestInputStdin -> "< " ++ infilename
      TestInputS3 -> "-r us-east-1 -b " ++ s3Bucket ++ " -k " ++ s3TestDataKey sessionId infilename

-- Create the local filepath where the test data is stored
inputDataFilename :: String -> String
inputDataFilename testId = "test-" ++ testId ++ ".jsonl"

-- Construct the filename for the output for a given test
resultsFilename :: String -> TestInputType -> String
resultsFilename testId testInputType = "test-" ++ testId ++ "-" ++ inputStr ++ ".result"
  where
    inputStr = case testInputType of
      TestInputFile -> "filein"
      TestInputStdin -> "stdin"
      TestInputS3 -> "s3in"

-- Helper function to create the local filpath from a filename
localInputDataFilepath :: String -> String
localInputDataFilepath = (localTestDataDir ++)

-- Helper function to create the local filpath from a filename
localResultsFilepath :: String -> String
localResultsFilepath = (localResultsDir ++)

-- Construct the local filepath where the golden file is found for a given test
localGoldenFilepath :: String -> String
localGoldenFilepath testId = localTestDataDir ++ "test-" ++ testId ++ ".golden"

-- Create the S3 key where the test data will be located (once paired with a bucket)
s3TestDataKey :: String -> String -> String
s3TestDataKey sessionId filename = s3TestDataDir ++ sessionId ++ "/" ++ filename
