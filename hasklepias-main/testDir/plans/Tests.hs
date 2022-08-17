{-
Test cases for P0000
-}
module Tests where

import           Hasklepias
import           Cohort

tests :: TestTree
tests = testGroup 
    "add tests" 
    [testCase "true is true" (True @?= True) ]