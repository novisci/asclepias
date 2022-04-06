{-| Tests of the example filter application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           ConstructPaths
import           Control.Concurrent
import           Control.Exception             ( catch
                                               , throwIO )
import qualified Data.ByteString.Char8         as B
import           Hasklepias
import           Hasklepias.ExampleFilterApp
import           TestUtils.BuildLargeTestData
import           TestUtils.SessionId
import           TestUtils.TestCases            ( TestInputType(..) )
import           TestUtils.S3Utils
import           System.Directory               ( createDirectoryIfMissing
                                                , removeDirectoryRecursive
                                                , removePathForcibly
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

main :: IO ()
main = do

  -- Generate a unique session ID and ensure that the results directory exists
  sessionId <- getSessionId
  createDirectoryIfMissing True localResultsDir

  -- Generate the "many subjects" test data and corresponding golden files.
  --
  -- TODO: there is currently no "many events per subject" test generation. It
  -- would be nice to add this group of test scenarios
  --
  -- TODO: it would be better to store this enumeration of transformed test
  -- cases (i.e the (1, 1, 1), (1, 1, 2), ...) so that it can be reused when
  -- constructing `testIds`
  generateManySubjectsTestAndGolden 1 1 1
  generateManySubjectsTestAndGolden 1 1 2
  generateManySubjectsTestAndGolden 1 1 3
  generateManySubjectsTestAndGolden 1 1 4

  -- Copy the test data to S3 with session-specific keys to avoid collisions
  mapM_ (writeTestDataToS3 sessionId) testIds

  -- Run the tests and perform cleanup. Note that ANY CODE WRITTEN AFTER THIS
  -- EXPRESION WILL BE SILENTLY IGNORED
  let tests' = testGroup "Tests of exampleFilterApp" (createTests sessionId)
  defaultMain tests'
    `catch` (\e -> do
      removeDirectoryRecursive localResultsDir
      let removeManySubjectsFileTest = removePathForcibly . createFilepathForTest
      let removeManySubjectsFileGolden = removePathForcibly . createFilepathForGolden
      removeManySubjectsFileTest (createLargeNewId 1 1 1)
      removeManySubjectsFileTest (createLargeNewId 1 1 2)
      removeManySubjectsFileTest (createLargeNewId 1 1 3)
      removeManySubjectsFileTest (createLargeNewId 1 1 4)
      removeManySubjectsFileGolden (createLargeNewId 1 1 1)
      removeManySubjectsFileGolden (createLargeNewId 1 1 2)
      removeManySubjectsFileGolden (createLargeNewId 1 1 3)
      removeManySubjectsFileGolden (createLargeNewId 1 1 4)
      s3RecursiveRm  ("s3://" ++ s3Bucket ++ "/" ++ s3TestDataDir)
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

-- Create a list of strings with each element representing a fragment of a
-- filename for a given test scenario
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

-- Create a collection of tests by obtaining the cartesian product of all of the
-- (i) test data inputs and (ii) sources of inputs
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
    (createFilepathForGolden id)
    (createFilepathForResults id testInputType)
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
      TestInputFile -> "-f " ++ createFilepathForTest id
      TestInputStdin -> "< " ++ createFilepathForTest id
      TestInputS3 -> "-r us-east-1 -b " ++ s3Bucket ++ " -k " ++ createS3KeyForTest sessionId id
    outfilename = createFilepathForResults id testInputType

-- Helper function to create the local filpath from a filename
localResultsFilepath :: String -> String
localResultsFilepath = (localResultsDir ++)

-- Copy test data to S3
writeTestDataToS3 :: String -> String -> IO ()
writeTestDataToS3 sessionId id = pure cmd >>= callCommand where
  from = createFilepathForTest id
  to   = createS3UriForTest sessionId id
  cmd  = "aws s3 cp " ++ from ++ " " ++ to

-- Generates the "many subjects" test data and golden file by replicating the
-- subjects in the existing test data and golden file for the n-m-v scenario
-- (and giving the generated subjects unique IDs). The number of replications is
-- fixed by `generateTestDataManySubjects`
generateManySubjectsTestAndGolden :: Int -> Int -> Int -> IO ()
generateManySubjectsTestAndGolden n m v = do
  let id = createId n m v
  let newId = createLargeNewId n m v
  generateTestDataManySubjects
    (createFilepathForTest id)
    (createFilepathForTest newId)
  generateTestDataManySubjects
    (createFilepathForGolden id)
    (createFilepathForGolden newId)

createId :: Int -> Int -> Int -> String
createId = printf "%d-%d-%d"

createNewId :: Int -> Int -> Int -> Int -> String
createNewId largeInputSize n m v =
  printf
    "%d-%d-%d"
    (largeInputSize * n)
    (largeInputSize * m)
    v

createLargeNewId :: Int -> Int -> Int -> String
createLargeNewId = createNewId largeInputSize
