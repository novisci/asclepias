{-|
Module      : Testing the event data model
-}

{-# LANGUAGE OverloadedStrings #-}

module EventData.Tests
  ( edmTests
  ) where

import           EventData.ClaimsSchema.Tests
import           Test.Tasty

edmTests :: IO ()
edmTests =
  defaultMain . testGroup "Event Model tests" =<< sequenceA [claimsSchemaTests]
