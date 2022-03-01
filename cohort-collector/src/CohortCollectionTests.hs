module CohortCollectionTests
  ( testsMain
  ) where

import           CohortCollection               ( Location(..)
                                                , runCollectionApp
                                                )
import qualified Data.ByteString.Lazy          as B
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Silver

appTestRw :: IO ()
appTestRw = do
  r <- runCollectionApp
    [ Local "test/tests/testrw1.json"
    , Local "test/tests/testrw2.json"
    , Local "test/tests/testrw3.json"
    ]
  B.writeFile "test/tests/testrw.json" r

appTestCw :: IO ()
appTestCw = do
  r <- runCollectionApp
    [ Local "test/tests/testcw1.json"
    , Local "test/tests/testcw2.json"
    , Local "test/tests/testcw3.json"
    ]
  B.writeFile "test/tests/testcw.json" r

tests :: TestTree
tests = testGroup
  "Tests of cohort collection"
  [ goldenVsFile "Collection of row-wise cohorts"
                 "test/tests/testrw.golden"
                 "test/tests/testrw.json"
                 appTestRw
  , goldenVsFile "Collection of column-wise cohorts"
                 "test/tests/testcw.golden"
                 "test/tests/testcw.json"
                 appTestCw
  ]

testsMain :: IO ()
testsMain = defaultMain tests
