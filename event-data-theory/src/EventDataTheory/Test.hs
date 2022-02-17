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
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , eitherDecode
                                                , decode
                                                , encode
                                                )
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Either                    ( fromLeft
                                                , isLeft
                                                , isRight
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           EventDataTheory.Core           ( Event
                                                , SubjectID
                                                )
import           EventDataTheory.EventLines     ( EventLine, eitherDecodeEvent )
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
import           Test.Tasty.HUnit               ( assertBool
                                                , testCase
                                                , (@?=)
                                                )
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
using 'createDecodeSmokeTestDecode'
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
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => FilePath
  -> IO TestTree
eventDecodeTests dir = createDecodeSmokeTestGroup
  ("Checking that .jsonl files in " <> dir <> " can be decoded")
  (\x -> (fromLeft "" x, isRight x))
  (eitherDecodeEvent @d @c @a)
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
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     )
  => FilePath
  -> IO TestTree
eventDecodeFailTests dir = createDecodeSmokeTestGroup
  ("Checking that .jsonl files in " <> dir <> " fail to decode")
  (\x -> ("successly parsed; should fail", isLeft x))
  (eitherDecodeEvent @d @c @a)
  [".jsonl"]
  dir



{-|
Creates a single test case
which decodes the contents of a file.
The test passes if the decoding then encoding 
results in the same bytestring contained in the file.
-}
createJSONRoundtripSmokeTest 
  :: forall a . (FromJSON a, ToJSON a) => 
     FilePath -- ^ path to file to be decoded
  -> IO TestTree
createJSONRoundtripSmokeTest testFile = do
  x <- B.readFile testFile
  let res = encode (decode @a x)
  let assert = res @?= x 
  pure $ testCase (takeBaseName testFile) (assert)

{-|
Creates a group of tests 
using 'createJSONRoundtripSmokeTest'
from all the files 
in given directory
with given file extensions.
-}

createJSONRoundtripSmokeTestGroup 
   :: forall a . (FromJSON a, ToJSON a) => 
    TestName
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
  :: forall d c a b
   . ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     , ToJSON a
     , ToJSON c
     , ToJSON d
     )
  => FilePath
  -> IO TestTree
eventLineRoundTripTests dir = createJSONRoundtripSmokeTestGroup 
  @(EventLine d c a)
  ("Checking that .jsonl files in " <> dir <> " can be decoded then encoded as EventLines")
  [".jsonl"]
  dir
