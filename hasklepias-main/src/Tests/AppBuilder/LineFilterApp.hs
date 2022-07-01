{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.AppBuilder.LineFilterApp where

import qualified Control.Foldl                 as L
import           Data.Aeson
import qualified Data.ByteString.Char8         as C
import           Data.Conduit
import           Data.String
import           Data.String.Interpolate        ( i )
import           Data.Text
import           Data.Vector                    ( (!) )
import           Hasklepias.AppBuilder.LineFilterApp
import           Hasklepias.AppBuilder.LineFilterApp.LineFilterLogic
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

dci = decodeStrict' @LineAppTesterID
dcl = decodeStrict' @LineAppTester
tpr = (== MkLineAppTester True)


{-
      Test application

      Used in lineFilter-test/Main.hs
-}
testFilterAppC :: IO ()
testFilterAppC =
  makeLineFilterApp filterAppC' "Test Line Filter conduit" dci dcl tpr

testFilterAppF :: IO ()
testFilterAppF =
  makeLineFilterApp filterAppFold' "Test Line Filter conduit" dci dcl tpr

{-
      Test values constructors
-}

mkTestInput :: Int -> Text -> C.ByteString
mkTestInput y x = [i|[#{ show y }, #{ x }]|]

mkTestLines :: [(Int, Text)] -> C.ByteString
mkTestLines x = C.intercalate "\n" (fmap (uncurry mkTestInput) x)

passLine = flip mkTestInput "true"
failLine = flip mkTestInput "false"
badLine = flip mkTestInput "1"

unline :: (Semigroup a, IsString a, Eq a) => a -> a -> a
unline x y = if x == "" then y else x <> "\n" <> y

-- mkLines n x = F.foldl' unline "" (Prelude.replicate n x)
mkLines n = C.unlines . Prelude.replicate n
passLines n x = mkLines n (passLine x)
failLines n x = mkLines n (failLine x)
badLines n x = mkLines n (badLine x)

nFail1pass n x = C.concat [failLines n x, passLines 1 x]


mkGroupLines x = C.concat $ Prelude.zipWith (\(f, i) -> f i) x [1 ..]

{-
      Benchmarks
-}

listInput :: Int -> [C.ByteString]
listInput n =
  uncurry mkTestInput <$> Prelude.replicate n (1 :: Int, "0" :: Text)

conduitApp x = runConduitPure (filterAppC dci dcl tpr x)

foldApp = L.fold (filterAppFold dci dcl tpr) . C.lines

appBenchInputs n m = mkGroupLines (Prelude.replicate n (nFail1pass, m))

appBenchInputsFail n m = mkGroupLines (Prelude.replicate n (failLines, m))

benches =
  [ bgroup
    "LineFilter Group-level fold"
    [ bench "100 elements" $ nf folder (listInput 100)
    , bench "1000 elements" $ nf folder (listInput 1000)
    , bench "10000 elements" $ nf folder (listInput 10000)
    ]
  , bgroup
    "LineFilter app comparision many groups/few lines"
    [ bench "conduit-based" $ nf conduitApp (appBenchInputs 10000 100)
    , bench "foldl-based" $ nf foldApp (appBenchInputs 10000 100)
    ]
  , bgroup
    "LineFilter app comparision few groups/many lines"
    [ bench "conduit-based" $ nf conduitApp (appBenchInputs 100 10000)
    , bench "foldl-based" $ nf foldApp (appBenchInputs 100 10000)
    ]
  ]
  where folder = L.fold (filterGroupFold dcl tpr)

-- These files were created in a ghci session for benchmarking externally,
-- using hyperfine.
-- file1 = C.writeFile "lineFilter-test/100000groups-10lines.jsonl" (appBenchInputs 100000 10)
-- file2 = C.writeFile "lineFilter-test/10000groups-100lines.jsonl" (appBenchInputs 10000 100)
-- file3 = C.writeFile "lineFilter-test/1000groups-1000lines.jsonl" (appBenchInputs 1000 1000)
-- file4 = C.writeFile "lineFilter-test/100groups-10000lines.jsonl" (appBenchInputs 100 10000)
-- file5 = C.writeFile "lineFilter-test/10groups-100000lines.jsonl" (appBenchInputs 10 100000)

-- file1 = C.writeFile "lineFilter-test/100000groups-10lines-allfail.jsonl" (appBenchInputsFail 100000 10)
-- file2 = C.writeFile "lineFilter-test/10000groups-100lines-allfail.jsonl" (appBenchInputsFail 10000 100)
-- file3 = C.writeFile "lineFilter-test/1000groups-1000lines-allfail.jsonl" (appBenchInputsFail 1000 1000)
-- file4 = C.writeFile "lineFilter-test/100groups-10000lines-allfail.jsonl" (appBenchInputsFail 100 10000)
-- file5 = C.writeFile "lineFilter-test/10groups-100000lines-allfail.jsonl" (appBenchInputsFail 10 100000)

{-
      Tests
-}

testCases =
  [ ("no input"      , ""           , "")
  , ("1 passing line", passLines 1 1, passLines 1 1)
  , ("1 failing line", failLines 1 1, "")
  , ("1 bad line"    , badLines 1 1 , "")
  , ( "1 group - with bad line"
    , C.concat [passLines 1 1, badLines 1 1]
    , C.concat [passLines 1 1, badLines 1 1]
    )
  , ( "1 group - 1 pass group"
    , mkGroupLines [(nFail1pass, 10)]
    , mkGroupLines [(nFail1pass, 10)]
    )
  , ( "2 groups - 2 pass groups"
    , mkGroupLines [(nFail1pass, 10)]
    , mkGroupLines [(nFail1pass, 10)]
    )
  , ( "2 groups - 1 pass group"
    , mkGroupLines [(nFail1pass, 10), (failLines, 10)]
    , nFail1pass 10 1
    )
  , ( "2 groups - 1 pass group"
    , mkGroupLines [(failLines, 10), (nFail1pass, 10)]
    , nFail1pass 10 2
    )
  , ( "2 groups - 01 pass group"
    , mkGroupLines [(failLines, 10), (failLines, 10)]
    , ""
    )
  ]

tests = testGroup
  "filter lines application"
  [ testGroup "conduit app" $ makeTests conduitApp testCases
  , testGroup "foldl app" $ makeTests foldApp testCases
  ]
  where makeTests f = fmap (\(n, i, r) -> testCase n $ f i @?= r)
