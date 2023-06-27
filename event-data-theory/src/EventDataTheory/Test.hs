{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Hasklepias Event Tests
-- Description : Provides test making functions for event models
module EventDataTheory.Test
  ( eventDecodeTests,
    eventDecodeFailTests,
    eventLineRoundTripTests,
    eventLineModifyTests,
    eventOrdTests,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    decode,
    decode',
    eitherDecode,
    encode,
  )
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Data
import Data.Either (fromLeft, isLeft, isRight)
import qualified Data.List as L
import Data.Text (Text)
import Data.Time (Day)
import EventDataTheory.Core
import EventDataTheory.EventLines
import GHC.Generics (Generic)
import IntervalAlgebra
import System.FilePath (FilePath, takeBaseName)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Silver (findByExtension)
import Type.Reflection (Typeable)

-- |
-- Creates a group of tests
-- using 'createDecodeSmokeTest'
-- from all the files
-- in given directory
-- with given file extensions.
-- Each test passes if the decoding results in a @Right a@ value.
createDecodeSmokeTestGroup ::
  -- | name to give this group of tests
  TestName ->
  -- | a function which tests the decoding result, first value is the string if test fails
  (Either String a -> (String, Bool)) ->
  -- | a function which decodes a @ByteString@ to @Either String a@
  (C.ByteString -> Either String a) ->
  -- | a list of file extensions to find in the provided directory
  [FilePath] ->
  -- | name of directory containing files to be parsed
  FilePath ->
  TestTree
createDecodeSmokeTestGroup n testf decoder exts dir = testCaseSteps n $ \step -> do
  sources <- findByExtension exts dir
  mapM_ (\f -> step f >> createDecodeAssertion f) sources
  where createDecodeAssertion testFile = uncurry assertBool . testf . decoder =<<  C.readFile testFile

-- |
-- Creates a group of tests
-- from all the files
-- in given directory
-- for all files ending in '.jsonl'.
-- Each file should contain 1 line containing 1 event.
-- Each test passes if the decoding results in a @Right a@ value.
--
-- The test group is meant as a smoke test
-- to check that events you think should parse
-- do in fact parse.
eventDecodeTests ::
  forall m t a b.
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a) =>
  FilePath ->
  TestTree
eventDecodeTests dir =
  createDecodeSmokeTestGroup
    ("Checking that .jsonl files in " <> dir <> " can be decoded")
    (\x -> (fromLeft "" x, isRight x))
    (eitherDecodeEvent' @m @t @a AddMomentAndFix)
    [".jsonl"]
    dir

-- |
-- Creates a group of tests
-- from all the files
-- in given directory
-- for all files ending in '.jsonl'.
-- Each file should contain 1 line containing 1 event.
-- Each test passes if the decoding results in a @Left String@ value.
--
-- The test group is meant as a smoke test
-- to check that events you think should _not_ parse
-- do not in fact parse.
eventDecodeFailTests ::
  forall m t a b.
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a) =>
  FilePath ->
  TestTree
eventDecodeFailTests dir =
  createDecodeSmokeTestGroup
    ("Checking that .jsonl files in " <> dir <> " fail to decode")
    (\x -> ("successly parsed; should fail", isLeft x))
    (eitherDecodeEvent' @m @t @a AddMomentAndFix)
    [".jsonl"]
    dir

-- |
-- Creates a single test case
-- which decodes the contents of a file.
-- The test passes if the decoding
-- then encoding and decoding again
-- results in the same value.
createJSONRoundtripSmokeTest ::
  forall a.
  (FromJSON a, ToJSON a, Eq a, Show a) =>
  -- | path to file to be decoded
  FilePath ->
  IO ()
createJSONRoundtripSmokeTest testFile = do
  x <- B.readFile testFile
  let d1 = decode' @a x
  let d2 = decode' @a (encode d1)
  d1 @?= d2

-- |
-- Creates a group of tests
-- using 'createJSONRoundtripSmokeTest'
-- from all the files
-- in given directory
-- with given file extensions.
createJSONRoundtripSmokeTestGroup ::
  forall a.
  (FromJSON a, ToJSON a, Eq a, Show a) =>
  TestName ->
  -- | a list of file extensions to find in the provided directory
  [FilePath] ->
  -- | name of directory containing files to be parsed
  FilePath ->
  TestTree
createJSONRoundtripSmokeTestGroup n exts dir = testCaseSteps n $ \step -> do
  sources <- findByExtension exts dir
  mapM_ (\f -> step f >> createJSONRoundtripSmokeTest @a f) sources

-- |
-- Creates a group of tests
-- from all the files
-- in given directory
-- for all files ending in '.jsonl'.
-- Each file should contain 1 line containing 1 event.
-- Each test passes if the decoding results in a @Right a@ value.
--
-- The test group is meant as a smoke test
-- to check that events you think should parse
-- do in fact parse.
eventLineRoundTripTests ::
  forall m t a b.
  ( Eventable t m a,
    FromJSONEvent t m a,
    ToJSONEvent t m a,
    SizedIv (Interval a)
  ) =>
  FilePath ->
  TestTree
eventLineRoundTripTests dir =
  createJSONRoundtripSmokeTestGroup @(EventLine t m a)
    ( "Checking that .jsonl files in "
        <> dir
        <> " can be decoded then encoded as EventLines"
    )
    [".jsonl"]
    dir

-- |
-- Creates a single test case
-- which decodes the contents of a file.
--
-- The test passes if:
--   * the file decodes into an 'EventLine'
--   * the result can be passed through 'modifyEventLineWithContext'
--     using the identity function without modifying the result.
createModifyEventLineTest ::
  forall m t a b.
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a, Data m) =>
  -- | path to file to be decoded
  FilePath ->
  IO ()
createModifyEventLineTest testFile = do
  x <- B.readFile testFile
  let d1 = decode' @(EventLine t m a) x

  case d1 of
        Nothing ->
          assertFailure ("failed to parse contents of " <> takeBaseName testFile)
        Just el -> do
          let d2 =
                modifyEventLineWithContext @m @m @t @t @a
                  AddMomentAndFix
                  id
                  x
          case d2 of
            Left s -> assertFailure s
            Right el' -> el @?= el'

-- |
-- Creates a group of tests
-- using 'createModifyEventLineTest'
-- from all the files
-- in given directory
-- with given file extensions.
createModifyEventLineTestGroup ::
  forall m t a b.
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a, Data m) =>
  TestName ->
  -- | a list of file extensions to find in the provided directory
  [FilePath] ->
  -- | name of directory containing files to be parsed
  FilePath ->
  TestTree
createModifyEventLineTestGroup n exts dir = testCaseSteps n $ \step -> do
  sources <- findByExtension exts dir
  mapM_ (\f -> step f >> createModifyEventLineTest @m @t @a f) sources

-- |
-- Creates a group of tests
-- from all the files
-- in given directory
-- for all files ending in '.jsonl'.
-- Each file should contain 1 line containing 1 event.
--
-- Tests pass if:
--   * the file decodes into an 'EventLine'
--   * the result can be passed through 'modifyEventLineWithContext'
--     using the identity function without modifying the result.
--
-- NOTE:
-- Tags need to be ordered within the JSON files.
-- The underlying type of @TagSet@ is @Set@,
-- thus elements will be ordered by their @Ord@ instance
-- in the result.
eventLineModifyTests ::
  forall m t a b.
  (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a, Data m) =>
  FilePath ->
  TestTree
eventLineModifyTests dir =
  createModifyEventLineTestGroup @m @t @a
    ( "Checking that .jsonl files in "
        <> dir
        <> " are not modified by modifyEventLine with the id function"
    )
    [".jsonl"]
    dir

-- | Events to use with sorting tests. Note these are all just
-- moment-length intervals with none overlapping. That's fine: interval-algebra is responsible for the
-- Ord instance on intervals themselves.
exampleEvents :: [[Event Int () Int]]
exampleEvents = map eventList $ L.permutations [0 .. 5]
  where
    ctx t = context (packTagSet [t]) () Nothing
    eventList xs = [event (beginervalMoment i) (ctx 0) | i <- xs]

-- | These are sorted by tagset.
exampleEvents' :: [[Event Int () Int]]
exampleEvents' = map eventList $ L.permutations [0 .. 5]
  where
    ctx t = context (packTagSet [t]) () Nothing
    eventList xs = [event (beginervalMoment 0) (ctx i) | i <- xs]

-- | Sorting any sublist above should give this.
sortedEvents :: [Event Int () Int]
sortedEvents = [event (beginervalMoment i) (ctx 0) | i <- [0 .. 5]]
  where
    ctx t = context (packTagSet [t]) () Nothing

sortedEvents' :: [Event Int () Int]
sortedEvents' = [event (beginervalMoment 0) (ctx i) | i <- [0 .. 5]]
  where
    ctx t = context (packTagSet [t]) () Nothing

-- | Tests of Ord instance for Event.
eventOrdTests :: TestTree
eventOrdTests =
  testGroup
    "Ord instance for Event"
    $ ivord ++ tagord
  where
    ivord = map (\ivs -> testCase "Sort by intervals using 'compare'" $ L.sort ivs @?= sortedEvents) exampleEvents
    tagord = map (\ivs -> testCase "Sort by tag set" $ L.sort ivs @?= sortedEvents') exampleEvents'
