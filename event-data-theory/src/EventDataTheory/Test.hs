{-|
Module      : Hasklepias Event Tests
Description : Provides test making functions for event models
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module EventDataTheory.Test
  ( eventDecodeTests
  , eventDecodeFailTests
  , eventLineRoundTripTests
  , eventLineModifyTests
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , decode
                                                , decode'
                                                , eitherDecode
                                                , encode
                                                )
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Data
import           Data.Either                    ( fromLeft
                                                , isLeft
                                                , isRight
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           EventDataTheory.Core           ( Event
                                                , Eventable
                                                , FromJSONEvent
                                                , SubjectID
                                                , ToJSONEvent
                                                )
import           EventDataTheory.EventLines
import           GHC.Generics                   ( Generic )
import           IntervalAlgebra                ( Interval
                                                , IntervalSizeable
                                                )
import           System.FilePath                ( FilePath
                                                , takeBaseName
                                                )
import           Test.Tasty                     ( TestName
                                                , TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit
import           Test.Tasty.Silver              ( findByExtension )
import           Type.Reflection                ( Typeable )

{-|
Creates a single test case
which decodes the contents of a file 
into @Either String a@.
The test passes if the decoding results in a @Right a@ value.
-}
createDecodeSmokeTest
  :: (Either String a -> (String, Bool))
  -- ^ a function which tests the decoding result, first value is the string if test fails
  -> (B.ByteString -> Either String a)
  -- ^ a function which decodes a @ByteString@ to @Either String a@
  -> FilePath -- ^ path to file to be decoded
  -> IO TestTree
createDecodeSmokeTest testf decoder testFile = do
  z <- decoder <$> B.readFile testFile
  let res = uncurry assertBool (testf z)
  pure $ testCase (takeBaseName testFile) res

{-|
Creates a group of tests 
using 'createDecodeSmokeTest'
from all the files 
in given directory
with given file extensions.
Each test passes if the decoding results in a @Right a@ value.
-}
createDecodeSmokeTestGroup
  :: TestName -- ^ name to give this group of tests
  -> (Either String a -> (String, Bool))
  -- ^ a function which tests the decoding result, first value is the string if test fails
  -> (B.ByteString -> Either String a)
  -- ^ a function which decodes a @ByteString@ to @Either String a@
  -> [FilePath] -- ^ a list of file extensions to find in the provided directory
  -> FilePath -- ^ name of directory containing files to be parsed
  -> IO TestTree
createDecodeSmokeTestGroup n testf decoder exts dir = do
  sources <- findByExtension exts dir
  let tests = traverse (createDecodeSmokeTest testf decoder) sources
  testGroup n <$> tests

{-|
Creates a group of tests 
from all the files 
in given directory
for all files ending in '.jsonl'.
Each file should contain 1 line containing 1 event.
Each test passes if the decoding results in a @Right a@ value.

The test group is meant as a smoke test
to check that events you think should parse 
do in fact parse. 
-}
eventDecodeTests
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a)
  => FilePath
  -> IO TestTree
eventDecodeTests dir = createDecodeSmokeTestGroup
  ("Checking that .jsonl files in " <> dir <> " can be decoded")
  (\x -> (fromLeft "" x, isRight x))
  (eitherDecodeEvent @m @t @a defaultParseEventLineOption)
  [".jsonl"]
  dir

{-|
Creates a group of tests 
from all the files 
in given directory
for all files ending in '.jsonl'.
Each file should contain 1 line containing 1 event.
Each test passes if the decoding results in a @Left String@ value.

The test group is meant as a smoke test
to check that events you think should _not_ parse 
do not in fact parse. 
-}
eventDecodeFailTests
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a)
  => FilePath
  -> IO TestTree
eventDecodeFailTests dir = createDecodeSmokeTestGroup
  ("Checking that .jsonl files in " <> dir <> " fail to decode")
  (\x -> ("successly parsed; should fail", isLeft x))
  (eitherDecodeEvent @m @t @a defaultParseEventLineOption)
  [".jsonl"]
  dir

{-|
Creates a single test case
which decodes the contents of a file.
The test passes if the decoding 
then encoding and decoding again
results in the same value.
-}
createJSONRoundtripSmokeTest
  :: forall a
   . (FromJSON a, ToJSON a, Eq a, Show a)
  => FilePath -- ^ path to file to be decoded
  -> IO TestTree
createJSONRoundtripSmokeTest testFile = do
  x <- B.readFile testFile
  let d1     = decode' @a x
  let d2     = decode' @a (encode d1)
  -- print d1 -- Just visually confirming d1 and d2 are equivalent
  -- print d2
  let assert = d1 @?= d2
  pure $ testCase (takeBaseName testFile) assert

{-|
Creates a group of tests 
using 'createJSONRoundtripSmokeTest'
from all the files 
in given directory
with given file extensions.
-}
createJSONRoundtripSmokeTestGroup
  :: forall a
   . (FromJSON a, ToJSON a, Eq a, Show a)
  => TestName
  -> [FilePath] -- ^ a list of file extensions to find in the provided directory
  -> FilePath -- ^ name of directory containing files to be parsed
  -> IO TestTree
createJSONRoundtripSmokeTestGroup n exts dir = do
  sources <- findByExtension exts dir
  let tests = traverse (createJSONRoundtripSmokeTest @a) sources
  testGroup n <$> tests

{-|
Creates a group of tests 
from all the files 
in given directory
for all files ending in '.jsonl'.
Each file should contain 1 line containing 1 event.
Each test passes if the decoding results in a @Right a@ value.

The test group is meant as a smoke test
to check that events you think should parse 
do in fact parse. 
-}
eventLineRoundTripTests
  :: forall m t a b
   . ( Eventable t m a
     , FromJSONEvent t m a
     , ToJSONEvent t m a
     , IntervalSizeable a b
     )
  => FilePath
  -> IO TestTree
eventLineRoundTripTests dir =
  createJSONRoundtripSmokeTestGroup @(EventLine t m a)
    (  "Checking that .jsonl files in "
    <> dir
    <> " can be decoded then encoded as EventLines"
    )
    [".jsonl"]
    dir


{-|
Creates a single test case
which decodes the contents of a file.

The test passes if:
  * the file decodes into an 'EventLine'
  * the result can be passed through 'modifyEventLineWithContext'
    using the identity function without modifying the result.
-}
createModifyEventLineTest
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a, Data m)
  => FilePath -- ^ path to file to be decoded
  -> IO TestTree
createModifyEventLineTest testFile = do
  x <- B.readFile testFile
  let d1 = decode' @(EventLine t m a) x

  let
    res = case d1 of
      Nothing ->
        assertFailure ("failed to parse contents of " <> takeBaseName testFile)
      Just el -> do
        let
          d2 = modifyEventLineWithContext @m @m @t @t @a
            defaultParseEventLineOption
            id
            x
        case d2 of
          Left  s   -> assertFailure s
          Right el' -> el @?= el'
  pure $ testCase (takeBaseName testFile) res


{-|
Creates a group of tests 
using 'createModifyEventLineTest'
from all the files 
in given directory
with given file extensions.
-}
createModifyEventLineTestGroup
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a, Data m)
  => TestName
  -> [FilePath] -- ^ a list of file extensions to find in the provided directory
  -> FilePath -- ^ name of directory containing files to be parsed
  -> IO TestTree
createModifyEventLineTestGroup n exts dir = do
  sources <- findByExtension exts dir
  let tests = traverse (createModifyEventLineTest @m @t @a) sources
  testGroup n <$> tests

{-|
Creates a group of tests 
from all the files 
in given directory
for all files ending in '.jsonl'.
Each file should contain 1 line containing 1 event.

Tests pass if:
  * the file decodes into an 'EventLine'
  * the result can be passed through 'modifyEventLineWithContext'
    using the identity function without modifying the result.

NOTE: 
Tags need to be ordered within the JSON files.
The underlying type of @TagSet@ is @Set@, 
thus elements will be ordered by their @Ord@ instance
in the result.
-}
eventLineModifyTests
  :: forall m t a b
   . (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a, Data m)
  => FilePath
  -> IO TestTree
eventLineModifyTests dir = createModifyEventLineTestGroup @m @t @a
  (  "Checking that .jsonl files in "
  <> dir
  <> " are not modified by modifyEventLine with the id function"
  )
  [".jsonl"]
  dir
