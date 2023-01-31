{-|
Module      : stype package tests
Description : for internal use
-}
module Stype.Tests
  ( stypeTests
  ) where

import qualified Stype.Tests.Aeson    as A
import qualified Stype.Tests.Censored as C
import           Test.Tasty

-- | All the tests of stype
stypeTests :: IO ()
stypeTests = defaultMain . testGroup "stype package tests" =<< sequenceA
  [pure A.tests, pure C.tests]
