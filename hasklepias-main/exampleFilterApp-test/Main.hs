{-| Tests of the example application
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Char8          as B
import           Hasklepias
import           Hasklepias.AppUtilities
import           Hasklepias.ExampleFilterApp
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Silver
import           System.IO
import           System.IO.Silently

testDir = "exampleFilterApp-test/test/"

runTest :: String -> IO ()
runTest x = do
    hFlush stdout -- flush stdout as test info sometimes being sent to file if running
                  -- tests with --test-show-details=always
    r <- capture $ runFilterAppWithLocation (Local (testDir <> "test-" <> x <> ".jsonl"))
                            exampleFilterApp

    hFlush stdout -- flush stdout as test info sometimes being sent to file if running
                  -- tests with --test-show-details=always      
    B.writeFile (testDir <> "test-" <> x <> ".result") ( snd r <> B.pack (fst r))


makeTest :: TestName -> String -> TestTree
makeTest n x =
  goldenVsFile
     n
    (testDir <> "test-" <> x <> ".golden")
    (testDir <> "test-" <> x <> ".result")
    (runTest x)

tests :: TestTree
tests = testGroup
  "Tests of exampleApp"
  [ makeTest "Test case 0-0-0" "0-0-0"
  , makeTest "Test case 1-0-1" "1-0-1"
  , makeTest "Test case 1-0-2" "1-0-2"
  , makeTest "Test case 1-0-3" "1-0-3"
  , makeTest "Test case 1-0-4" "1-0-4"
  , makeTest "Test case 1-0-5" "1-0-5"
  , makeTest "Test case 1-1-1" "1-1-1"
  , makeTest "Test case 1-1-2" "1-1-2"
  , makeTest "Test case 1-1-3" "1-1-3"
  ]

main :: IO ()
main = defaultMain tests
