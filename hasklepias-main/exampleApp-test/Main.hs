{-| Tests of the example application
-}

module Main 
  ( main
  ) where

import Hasklepias.ExampleApp
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Silver
import qualified Data.ByteString.Lazy          as B
import Hasklepias.MakeApp (runApp)

-- exampleAppTest :: IO ()
-- exampleAppTest = do
--   r <- runApp exampleApp "TODO"
--   B.writeFile "test/tests/testrw.json" r


tests :: TestTree
tests = testGroup
  "Tests of cohort collection"
  [ 
    -- goldenVsFile "ExampleApp of row-wise cohort"
    --              "test/tests/testrw.golden"
    --              "test/tests/testrw.json"
    --              exampleAppTest
--   , goldenVsFile "Collection of column-wise cohorts"
--                  "test/tests/testcw.golden"
--                  "test/tests/testcw.json"
--                  appTestCw
  ]

main :: IO ()
main = defaultMain tests