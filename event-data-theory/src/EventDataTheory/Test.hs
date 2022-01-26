{-|
Module      : Hasklepias Event Tests
Description : Provides test making functions for event models
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module EventDataTheory.Test
  ( EventSmokers
  ) where

import           Data.Aeson                     ( FromJSON )
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Either                    ( fromLeft
                                                , isRight
                                                )
import           Data.Time                      ( Day )
import           EventDataTheory.Core           ( Event
                                                , SubjectID
                                                )
import           EventDataTheory.EventLines     ( eitherDecodeEvent )
import           GHC.Generics                   ( Generic )
import           IntervalAlgebra                ( IntervalSizeable )
import           System.FilePath                ( takeBaseName, FilePath )
import           Test.Tasty                     ( TestName
                                                , TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( assertBool
                                                , testCase
                                                )
import           Test.Tasty.Silver              ( findByExtension )
import           Type.Reflection                ( Typeable )

{-|
Creates a single test case
which decodes the contents of a file 
into @Either String a@.
The test passes if the decoding results in a @Right a@ value.
-}
createDecodeSmokeTestDecode
  :: (B.ByteString -> Either String a) -- ^ a function which decodes a @ByteString@ to @Either String a@
  -> FilePath -- ^ path to file to be decoded
  -> IO TestTree
createDecodeSmokeTestDecode decoder testFile = do
  z <- decoder <$> B.readFile testFile
  let res = assertBool (fromLeft "" z) (isRight z)
  return $ testCase (takeBaseName testFile) res

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
  -> (B.ByteString -> Either String a) -- ^ a function which decodes a @ByteString@ to @Either String a@
  -> [FilePath] -- ^ a list of file extensions to find in the provided directory
  -> FilePath -- ^ name of directory containing files to be parsed
  -> IO TestTree
createDecodeSmokeTestGroup n decoder exts dir = do
  sources <- findByExtension exts dir
  let tests = traverse (createDecodeSmokeTestDecode decoder)
                       [ file | file <- sources ]
  testGroup n <$> tests

{-|
-}
class ( Show d
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
     ) => EventSmokers d c a b where
  eventDecodeTests :: FilePath -> IO TestTree
  eventDecodeTests x = createDecodeSmokeTestGroup
      (".jsonl files in " <> x <> "can be decoded")
      (eitherDecodeEvent :: (B.ByteString  -> Either String (SubjectID, Event d c a)))
      [".jsonl"]
      x

instance (Show d, Eq d, Generic d, FromJSON d
         , Show c, Eq c, Ord c, Typeable c, FromJSON c) =>
        EventSmokers d c Day Integer

instance (Show d, Eq d, Generic d, FromJSON d
         , Show c, Eq c, Ord c, Typeable c, FromJSON c) =>
        EventSmokers d c Integer Integer

instance (Show d, Eq d, Generic d, FromJSON d
         , Show c, Eq c, Ord c, Typeable c, FromJSON c) =>
        EventSmokers d c Int Int
