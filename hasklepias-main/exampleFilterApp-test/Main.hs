{-| Tests of the example filter application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Concurrent
import           Control.Exception             ( catch
                                               , throwIO )
import qualified Data.ByteString.Char8         as B
import           Hasklepias
import           Hasklepias.ExampleFilterApp
import           TestUtils.BuildLargeTestData
import           TestUtils.SessionId
import           System.Directory               ( createDirectoryIfMissing
                                                -- , removeDirectoryRecursive
                                                -- , removePathForcibly
                                                )
import           System.Exit                    ( ExitCode )
import           System.IO
import           System.IO.Silently
import           System.Process                 ( callCommand )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Silver
import           Text.Printf

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
s3TestDataDir = "hasklepias/sandbox-testapps/"

largeInputSize :: Int
largeInputSize = 1000

-- Enumeration of input sources
data TestInputType = TestInputFile | TestInputStdin | TestInputS3

main :: IO ()
main = do

  -- Generate a unique session ID and ensure that the results directory exists
  sessionId <- getSessionId
  createDirectoryIfMissing True localResultsDir

  -- Generate the "large" test data and corresponding golden files
  generateLargeTestAndGolden 1 1 1
  generateLargeTestAndGolden 1 1 2
  generateLargeTestAndGolden 1 1 3
  generateLargeTestAndGolden 1 1 4

  -- Copy the test data to S3 with session-specific keys to avoid collisions
  mapM_ (writeTestDataToS3 sessionId) testIds

  -- -- Copy the test data to S3 with session-specific keys to avoid collisions
  -- writeTestDataToS3 sessionId TestDataEmpty
  -- writeTestDataToS3 sessionId TestDataSmall
  -- writeTestDataToS3 sessionId TestDataManySubj
  -- writeTestDataToS3 sessionId TestDataManyEvent
  let tests' = testGroup "Tests of exampleFilterApp" (createTests sessionId)

  -- Run the tests and perform cleanup. Note that ANY CODE WRITTEN AFTER THIS
  -- EXPRESION WILL BE SILENTLY IGNORED
  defaultMain tests'
    `catch` (\e -> do
      -- removeDirectoryRecursive localResultsDir
      -- TODO: remove test-1000-1000-1.jsonl, test-1000-1000-1.golden, etc
      -- removeSessionDirFromS3 s3TestDataDir sessionId  -- TODO: uncomment!
      -- removeSessionDirFromS3 s3ResultsDir sessionId  -- TODO: uncomment!
      -- FIXME: add generated test files and golden files to gitignore?
      throwIO (e :: ExitCode))

-- runTest :: String -> IO ()
-- runTest x = do
--     -- The filter application writes to stdout as it goes, hence I use the
--     -- capture function to collect that output. Unfortunately, this has the
--     -- effect of adding some non-determinism to the test suite because it can
--     -- happen that the output of the test module itself is also captured.
--     -- Hence, I'm adding some stdout flushing and delays to try to prevent that.
--     -- FWIW, the filter app could probably be architected such that capture is
--     -- not needed, but that requires more willpower and knowledge than I have
--     -- at the moment. -- bsaul 2021-10-27
--   hFlush stdout -- flush stdout as test info sometimes being sent to file if running test
--   threadDelay 500 -- pause 500 microseconds
--   r <- capture $ runFilterAppWithLocation
--     (Local (localTestDataDir <> "test-" <> x <> ".jsonl"))
--     exampleFilterApp

--   hFlush stdout -- flush stdout as test info sometimes being sent to file if running tests
--   B.writeFile (localTestDataDir <> "test-" <> x <> ".result") (B.pack (fst r) <> snd r)

-- makeTest :: TestName -> String -> TestTree
-- makeTest n x = goldenVsFile n
--                             (localTestDataDir <> "test-" <> x <> ".golden")
--                             (localTestDataDir <> "test-" <> x <> ".result")
--                             (runTest x)

-- makeTests :: [String] -> [TestTree]
-- makeTests = fmap (\x -> makeTest ("Test case " <> x) x)

-- tests :: TestTree
-- tests = testGroup
--   "Tests of exampleFilterApp"
--   (makeTests testIds)

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
  , createLargeNewId 1 1 1
  , createLargeNewId 1 1 2
  , createLargeNewId 1 1 3
  , createLargeNewId 1 1 4
  ]

createTests :: String -> [TestTree]
createTests sessionId =
  concat nestedTests
  where
    testInputTypes = [TestInputFile, TestInputStdin, TestInputS3]
    testPtlByIds = map (appGoldenVsFile sessionId) testIds
    nestedTests = map (\f -> map f testInputTypes) testPtlByIds

-- Conduct a single test
appGoldenVsFile :: String -> String -> TestInputType -> TestTree
appGoldenVsFile sessionId id testInputType =
  goldenVsFile
    ("Test case " ++ id ++ " reading from " ++ inputStr)
    (createLocalFilepathForGolden id)
    (createLocalFilepathForResults id testInputType)
    (appTest sessionId id testInputType)
    where
      inputStr = case testInputType of
        TestInputFile -> "file"
        TestInputStdin -> "standard input"
        TestInputS3 -> "S3"

-- Build a shell command represented by string and run the command as a
-- subprocess, where the command is a cohort-building application
appTest :: String -> String -> TestInputType -> IO ()
appTest sessionId testId testInputType = do
  let outfilename = createFilenameForResults testId testInputType
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
appTestCmd sessionId id testInputType =
  "exampleFilterApp " ++ inputFragm ++ " > " ++ outfilename
  where
    inputFragm = case testInputType of
      TestInputFile -> "-f " ++ createLocalFilepathForTest id
      TestInputStdin -> "< " ++ createLocalFilepathForTest id
      TestInputS3 -> "-r us-east-1 -b " ++ s3Bucket ++ " -k " ++ createS3keyForTest sessionId id
    outfilename = createLocalFilepathForResults id testInputType

-- -- Create the local filepath where the test data is stored
-- inputDataFilename :: String -> String
-- inputDataFilename testId = "test-" ++ testId ++ ".jsonl"

-- -- Helper function to create the local filpath from a filename
-- localInputDataFilepath :: String -> String
-- localInputDataFilepath = (localTestDataDir ++)

-- Helper function to create the local filpath from a filename
localResultsFilepath :: String -> String
localResultsFilepath = (localResultsDir ++)

-- Copy test data to S3
writeTestDataToS3 :: String -> String -> IO ()
writeTestDataToS3 sessionId id = pure cmd >>= callCommand where
  from = createLocalFilepathForTest id
  to   = createS3uriForTest sessionId id
  cmd  = "aws s3 cp " ++ from ++ " " ++ to

-- -- the filesystem
-- generateTestDataManySubjectsWrp :: String -> String -> IO ()
-- generateTestDataManySubjectsWrp id newId =
-- generateTestDataManySubjects
--     (localTestDataDir ++ "test-" ++ id ++ ".jsonl")
--     (localTestDataDir ++ "test-" ++ newId ++ ".jsonl")

-- -- Construct the golden file for the "many subjects" test scenario and write it
-- -- to the filesystem
-- generateGoldenManySubjectsWrp :: String -> String -> IO ()
-- generateGoldenManySubjectsWrp id newId =
--   generateTestDataManySubjects
--     (localTestDataDir ++ "test-" ++ id ++ ".golden")
--     (localTestDataDir ++ "test-" ++ newId ++ ".golden")

generateLargeTestAndGolden :: Int -> Int -> Int -> IO ()
generateLargeTestAndGolden n m v = do
  let id = createId n m v
  let newId = createLargeNewId n m v
  generateTestDataManySubjects
    (createLocalFilepathForTest id)
    (createLocalFilepathForTest newId)
  generateTestDataManySubjects
    (createLocalFilepathForGolden id)
    (createLocalFilepathForGolden newId)

createFilenameForTest :: String -> String
createFilenameForTest id = "test-" ++ id ++ ".jsonl"

createFilenameForGolden :: String -> String
createFilenameForGolden id = "test-" ++ id ++ ".golden"

-- Construct the filename for the output for a given test
createFilenameForResults :: String -> TestInputType -> String
createFilenameForResults id testInputType =
  "result-"
    ++ id
    ++ "-"
    ++ inputStr
    ++ ".jsonl"
  where
    inputStr = case testInputType of
      TestInputFile -> "filein"
      TestInputStdin -> "stdin"
      TestInputS3 -> "s3in"

createLocalFilepathForTest :: String -> String
createLocalFilepathForTest id = localTestDataDir ++ createFilenameForTest id

createLocalFilepathForGolden :: String -> String
createLocalFilepathForGolden id = localTestDataDir ++ createFilenameForGolden id

createLocalFilepathForResults :: String -> TestInputType -> String
createLocalFilepathForResults id testInputType =
  localResultsDir ++ createFilenameForResults id testInputType

-- Create the S3 key where the test data will be located (once paired with a bucket)
createS3keyForTest :: String -> String -> String
createS3keyForTest sessionId id =
  s3TestDataDir
    ++ "filterApp/"
    ++ sessionId
    ++ "/testdata/"
    ++ createFilenameForTest id

-- Create the S3 URI where the test data will be located
createS3uriForTest sessionId id =
  "s3://"
    ++ s3Bucket
    ++ "/"
    ++ createS3keyForTest sessionId id

createId :: Int -> Int -> Int -> String
createId n m v = printf "%d-%d-%d" n m v

createNewId :: Int -> Int -> Int -> Int -> String
createNewId largeInputSize n m v =
  printf
    "%d-%d-%d"
    (largeInputSize * n)
    (largeInputSize * m)
    v

createLargeNewId :: Int -> Int -> Int -> String
createLargeNewId = createNewId largeInputSize
