{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tests.Features.Output
  ( tests
  ) where

import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as B
import           Data.Maybe
import           Features
import           Test.Tasty
import           Test.Tasty.HUnit


dummy :: Feature "dummy" Bool
dummy = pure True

instance HasAttributes "dummy" Bool where
  getAttributes x =
    MkAttributes "some Label" "longer label..." "a description" emptyPurpose

dummy2 :: Feature "dummy2" Bool
dummy2 = pure True

instance HasAttributes "dummy2" Bool where
  getAttributes x = emptyAttributes

tests :: TestTree
tests = testGroup
  "Unit tests on features outputs as JSON"
  [ testCase "dummy encodes correctly"
  $ encode dummy
  @?= "{\
        \\"attrs\":{\
            \\"getDerivation\":\"a description\",\
            \\"getLongLabel\":\"longer label...\",\
            \\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\
            \\"getShortLabel\":\"some Label\"\
            \},\
        \\"data\":true,\
        \\"name\":\"dummy\",\
        \\"type\":\"Bool\"\
        \}"
  , testCase "dummy2 encodes correctly"
  $ encode dummy2
  @?= "{\
        \\"attrs\":{\
            \\"getDerivation\":\"\",\
            \\"getLongLabel\":\"\",\
            \\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\
            \\"getShortLabel\":\"\"\
            \},\
        \\"data\":true,\
        \\"name\":\"dummy2\",\
        \\"type\":\"Bool\"\
        \}"
  ]
