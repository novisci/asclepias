{-# OPTIONS_HADDOCK hide #-}
{-|
Module      : Hasklepias core package tests
Description : for internal use
-}
module Tests
  ( coreTests
  ) where

import           Test.Tasty
import qualified Tests.Cohort                  as C
import qualified Tests.Features                as F

-- | All the tests of hasklepias-core
coreTests :: IO ()
coreTests = defaultMain . testGroup "Hasklepias core tests" =<< sequenceA
  [pure F.tests, pure C.tests]
