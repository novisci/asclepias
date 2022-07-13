{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Hasklepias.AppBuilder.ProcessLines.Tests where

import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Control.Exception              ( evaluate )

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , decode
                                                , decode'
                                                , decodeStrict
                                                , decodeStrict'
                                                , withArray
                                                )
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.String
import           Data.String.Interpolate        ( i )
import           Data.Text
import           Data.Vector                    ( (!) )
import           GHC.Exts
import           Hasklepias.AppBuilder.ProcessLines.Logic
import           Test.Tasty
import           Test.Tasty.Bench
import           Test.Tasty.HUnit

{-
      Types for testing
-}

newtype LineAppTesterID = MkLineAppTesterID Int deriving (Show, Eq)

instance FromJSON LineAppTesterID where
  parseJSON = withArray "FooID" $ \a -> do
    id <- parseJSON (a ! 0)
    pure $ MkLineAppTesterID id

newtype LineAppTester = MkLineAppTester Bool deriving (Show, Eq, Ord)

instance FromJSON LineAppTester where
  parseJSON = withArray "Foo" $ \a -> do
    id <- parseJSON (a ! 1)
    pure $ MkLineAppTester id

dciS' = decodeStrict' @LineAppTesterID
dclS' = decodeStrict' @LineAppTester
dciS = decodeStrict @LineAppTesterID
dclS = decodeStrict @LineAppTester

dciL' = decode' @LineAppTesterID
dclL' = decode' @LineAppTester
dciL = decode @LineAppTesterID
dclL = decode @LineAppTester

tpr = (== MkLineAppTester True)

{-
      Test values constructors
-}

mkTestInput :: Int -> Text -> BS.ByteString
mkTestInput y x = [i|[#{ show y }, #{ x }]|]

mkTestInputL :: Int -> Text -> BL.ByteString
mkTestInputL y x = [i|[#{ show y }, #{ x }]|]

mkTestLines x = BS.intercalate "\n" (fmap (uncurry mkTestInput) x)

-- Strict bytestrings
passLine = flip mkTestInput "true"
failLine = flip mkTestInput "false"
badLine = flip mkTestInput "1"

mkLines n = BS.unlines . Prelude.replicate n
passLines n x = mkLines n (passLine x)
failLines n x = mkLines n (failLine x)
badLines n x = mkLines n (badLine x)

nFailOnepass n x = BS.concat [failLines n x, passLines 1 x]
onePassNfail n x = BS.concat [passLines 1 x, failLines n x]

mkGroupLines x = BS.concat $ Prelude.zipWith (\(f, i) -> f i) x [1 ..]

-- Lazy btyestrings

passLineL = flip mkTestInputL "true"
failLineL = flip mkTestInputL "false"
badLineL = flip mkTestInputL "1"

mkLinesL n = BL.unlines . Prelude.replicate n
passLinesL n x = mkLinesL n (passLineL x)
failLinesL n x = mkLinesL n (failLineL x)
badLinesL n x = mkLinesL n (badLineL x)

nFailOnepassL n x = BL.concat [failLinesL n x, passLinesL 1 x]
onePassNfailL n x = BL.concat [passLinesL 1 x, failLinesL n x]

mkGroupLinesL x = BL.concat $ Prelude.zipWith (\(f, i) -> f i) x [1 ..]

{-
      Test cases
-}

groupTestCasesStrict =
  [ ("no input"        , ""            , "")
  , ("without newline at end", "[1,false]\n[1,true]", "[1,false]\n[1,true]")
  , ("1 passing line"  , passLines 1 1 , passLines 1 1)
  , ("2 passing lines" , passLines 2 1 , passLines 2 1)
  , ("10 passing lines", passLines 10 1, passLines 10 1)
  , ( "1 passing line - 1 bad line"
    , BS.concat [passLines 1 1, badLines 1 1]
    , BS.concat [passLines 1 1, badLines 1 1]
    )
  , ("1 failing line"  , failLines 1 1 , "")
  , ("2 failing lines" , failLines 2 1 , "")
  , ("10 failing lines", failLines 10 1, "")
  , ( "1 failing lines 1 pass line"
    , mkGroupLines [(nFailOnepass, 1)]
    , mkGroupLines [(nFailOnepass, 1)]
    )
  , ( "2 failing lines 1 pass line"
    , mkGroupLines [(nFailOnepass, 2)]
    , mkGroupLines [(nFailOnepass, 2)]
    )
  , ( "10 failing lines 1 pass line"
    , mkGroupLines [(nFailOnepass, 10)]
    , mkGroupLines [(nFailOnepass, 10)]
    )
  , ( "1 pass line 1 fail lines"
    , mkGroupLines [(onePassNfail, 1)]
    , mkGroupLines [(onePassNfail, 1)]
    )
  , ( "1 pass line 2 fail lines"
    , mkGroupLines [(onePassNfail, 2)]
    , mkGroupLines [(onePassNfail, 2)]
    )
  , ( "1 pass line 10 fail lines"
    , mkGroupLines [(onePassNfail, 10)]
    , mkGroupLines [(onePassNfail, 10)]
    )
  ]


groupTestCasesLazy =
  [ ("no input"        , ""            , "")
  , ("without newline at end", "[1,false]\n[1,true]", "[1,false]\n[1,true]")
  , ("1 passing line"  , passLinesL 1 1 , passLinesL 1 1)
  , ("2 passing lines" , passLinesL 2 1 , passLinesL 2 1)
  , ("10 passing lines", passLinesL 10 1, passLinesL 10 1)
  , ( "1 passing line - 1 bad line"
    , BL.concat [passLinesL 1 1, badLinesL 1 1]
    , BL.concat [passLinesL 1 1, badLinesL 1 1]
    )
  , ("1 failing line"  , failLinesL 1 1 , "")
  , ("2 failing lines" , failLinesL 2 1 , "")
  , ("10 failing lines", failLinesL 10 1, "")
  , ( "1 failing lines 1 pass line"
    , mkGroupLinesL [(nFailOnepassL, 1)]
    , mkGroupLinesL [(nFailOnepassL, 1)]
    )
  , ( "2 failing lines 1 pass line"
    , mkGroupLinesL [(nFailOnepassL, 2)]
    , mkGroupLinesL [(nFailOnepassL, 2)]
    )
  , ( "10 failing lines 1 pass line"
    , mkGroupLinesL [(nFailOnepassL, 10)]
    , mkGroupLinesL [(nFailOnepassL, 10)]
    )
  , ( "1 pass line 1 fail lines"
    , mkGroupLinesL [(onePassNfailL, 1)]
    , mkGroupLinesL [(onePassNfailL, 1)]
    )
  , ( "1 pass line 2 fail lines"
    , mkGroupLinesL [(onePassNfailL, 2)]
    , mkGroupLinesL [(onePassNfailL, 2)]
    )
  , ( "1 pass line 10 fail lines"
    , mkGroupLinesL [(onePassNfailL, 10)]
    , mkGroupLinesL [(onePassNfailL, 10)]
    )
  ]

appTestCases =
  [ ("no input"      , ""           , "")
  , ("1 passing line", passLines 1 1, passLines 1 1)
  , ("1 failing line", failLines 1 1, "")
  , ("1 bad line"    , badLines 1 1 , "")
  , ( "1 group - with bad line"
    , BS.concat [passLines 1 1, badLines 1 1]
    , BS.concat [passLines 1 1, badLines 1 1]
    )
  , ( "1 group - 1 pass group"
    , mkGroupLines [(nFailOnepass, 10)]
    , mkGroupLines [(nFailOnepass, 10)]
    )
  , ( "2 groups - 2 pass groups"
    , mkGroupLines [(nFailOnepass, 10)]
    , mkGroupLines [(nFailOnepass, 10)]
    )
  , ( "2 groups - 1 pass group"
    , mkGroupLines [(nFailOnepass, 10), (failLines, 10)]
    , nFailOnepass 10 1
    )
  , ( "2 groups - 1 pass group"
    , mkGroupLines [(failLines, 10), (nFailOnepass, 10)]
    , nFailOnepass 10 2
    )
  , ( "2 groups - 01 pass group"
    , mkGroupLines [(failLines, 10), (failLines, 10)]
    , ""
    )
  ]


{-
      Tests
-}

tests = testGroup
  "processing group lines"
  [ testGroup "processLinesStrict"
      $ makeTests (processGroupLinesStrict dclS' tpr) groupTestCasesStrict
  , testGroup "processLinesLazy"
      $ makeTests (processGroupLinesLazy dclL' tpr) groupTestCasesLazy
  ]
  where makeTests f = fmap (\(n, i, r) -> testCase n $ f i @?= r)
  -- "filter lines application"
  -- [
  -- --   -- testGroup "conduit app" $ makeTests conduitApp testCases
  -- --   testGroup "option A" $ makeTests (app_optionA . BS.lines) testCases
  -- -- , testGroup "option E" $ makeTests (app_optionE . BS.lines) testCases
  -- ]
  -- 
