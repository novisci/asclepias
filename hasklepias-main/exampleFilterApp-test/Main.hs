{-| Tests of the example filter application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Concurrent
import qualified Data.ByteString.Char8          as B
import           Hasklepias
import           Hasklepias.ExampleFilterApp
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Silver
import           System.IO
import           System.IO.Silently

{-
Test case file names follow this convention:

test-n-m-v.jsonl
     ^ ^ ^
     | | +--- version (e.g. for different numbers of events or permutations of the order of subject and/or their events)
     | +----- number of subject that do NOT have any event satisfying predicate
     +------- number of subject that *do* have some event satisfying predicate

-}


testDir = "exampleFilterApp-test/test/"

runTest :: String -> IO ()
runTest x = do
    hFlush stdout -- flush stdout as test info sometimes being sent to file if running test
    threadDelay 10 -- pause 10 microseconds
    r <- capture $ runFilterAppWithLocation (Local (testDir <> "test-" <> x <> ".jsonl"))
                            exampleFilterApp

    hFlush stdout -- flush stdout as test info sometimes being sent to file if running tests
    B.writeFile (testDir <> "test-" <> x <> ".result") (  B.pack (fst r) <> snd r )


makeTest :: TestName -> String -> TestTree
makeTest n x =
  goldenVsFile
     n
    (testDir <> "test-" <> x <> ".golden")
    (testDir <> "test-" <> x <> ".result")
    (runTest x)

makeTests :: [String] -> [TestTree]
makeTests = fmap (\x -> makeTest ("Test case " <> x) x )

tests :: TestTree
tests = testGroup
  "Tests of exampleFilterApp"
  ( makeTests [
      "0-0-1"
    , "0-1-1", "0-1-2"
    , "0-2-1"
    , "0-3-1"
    , "1-0-1", "1-0-2", "1-0-3", "1-0-4", "1-0-5"
    , "1-1-1", "1-1-2", "1-1-3", "1-1-4"
    , "1-2-1", "1-2-2", "1-2-3"
    , "2-0-1", "2-0-2", "2-0-3", "2-0-4"
    , "2-1-1", "2-1-2", "2-1-3", "2-1-4"
    , "3-0-1", "3-0-2", "3-0-3", "3-0-4"
  ] )

main :: IO ()
main = defaultMain tests
