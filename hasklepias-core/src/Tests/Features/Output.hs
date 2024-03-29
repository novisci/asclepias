{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Tests.Features.Output
  ( tests
  ) where

import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe
import           Features
import           Test.Tasty
import           Test.Tasty.HUnit


dummy :: Feature "dummy" Bool
dummy = pure True

dummy2 :: Feature "dummy2" Bool
dummy2 = pure True

setManyAttributes
  [ ("dummy2", ''Bool, emptyAttributes)
  , ("dummy", ''Bool, MkAttributes "some Label" "longer label..." "a description" emptyPurpose )]

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
    \\"getDerivation\":"
    <> derv
    <> ",\
            \\"getLongLabel\":"
    <> ll
    <> ",\
            \\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\
            \\"getShortLabel\":"
    <> sl
    <> "\
            \}"
o a d n t = "{" <> a <> "," <> d <> "," <> n <> "," <> t <> "}"

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
