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

{-
NOTE:
The following functions may need to modified depending on the version 
of Aeson being used.
-}

d = "\"data\":true"
n n = "\"name\":" <> n
t = "\"type\":\"Bool\""
a derv ll sl =
  "\"attrs\":{\
            \\"getShortLabel\":"
    <> sl
    <> ",\
            \\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\
            \\"getLongLabel\":"
    <> ll
    <> ",\
            \\"getDerivation\":"
    <> derv
    <> "\
            \}"
o a d n t = "{" <> n <> "," <> t <> "," <> a <> "," <> d <> "}"

tests :: TestTree
tests = testGroup
  "Unit tests on features outputs as JSON"
  [ testCase "dummy encodes correctly"
  $   encode dummy
  @?= o (a "\"a description\"" "\"longer label...\"" "\"some Label\"")
        d
        (n "\"dummy\"")
        t
  , testCase "dummy2 encodes correctly"
  $   encode dummy2
  @?= o (a "\"\"" "\"\"" "\"\"") d (n "\"dummy2\"") t
  ]
