{-| Tests of the collector application
-}

module Main 
  ( main
  ) where

-- import Main (tests)
import Hasklepias (runCollectionApp, Location(..))
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Silver
import qualified Data.ByteString.Lazy          as B

appTestRw :: IO ()
appTestRw = do
  r <- runCollectionApp
    [ Local "collector-test/tests/testrw1.json"
    , Local "collector-test/tests/testrw2.json"
    , Local "collector-test/tests/testrw3.json"
    ]
  B.writeFile "collector-test/tests/testrw.json" r

appTestCw :: IO ()
appTestCw = do
  r <- runCollectionApp
    [ Local "collector-test/tests/testcw1.json"
    , Local "collector-test/tests/testcw2.json"
    , Local "collector-test/tests/testcw3.json"
    ]
  B.writeFile "collector-test/tests/testcw.json" r

tests :: TestTree
tests = testGroup
  "Tests of cohort collection"
  [ goldenVsFile "Collection of row-wise cohorts"
                 "collector-test/tests/testrw.golden"
                 "collector-test/tests/testrw.json"
                 appTestRw
  , goldenVsFile "Collection of column-wise cohorts"
                 "collector-test/tests/testcw.golden"
                 "collector-test/tests/testcw.json"
                 appTestCw
  ]

main :: IO ()
main = defaultMain tests