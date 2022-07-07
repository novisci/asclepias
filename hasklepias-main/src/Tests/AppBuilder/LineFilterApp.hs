{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Tests.AppBuilder.LineFilterApp where

import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Control.Exception              ( evaluate )

import           Acc
import qualified Control.Foldl                 as L
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , decode
                                                , decode'
                                                , decodeStrict
                                                , decodeStrict'
                                                , withArray
                                                )
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Conduit
import           Data.Sequence           hiding ( fromList )
import           Data.String
import           Data.String.Interpolate        ( i )
import           Data.Text
import           Data.Vector                    ( (!) )
import           GHC.Exts
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
      Test application

      Used in lineFilter-test/Main{optionID}.hs
-}


testFilterAppA :: IO ()
testFilterAppA =
  makeLineFilterApp runProcessLinesApp_OptionA "Option A" dciS' dclS' tpr

testFilterAppC :: IO ()
testFilterAppC =
  makeLineFilterApp runProcessLinesApp_OptionC "Option C" dciS' dclS' tpr

testFilterAppE :: IO ()
testFilterAppE =
  makeLineFilterApp runProcessLinesApp_OptionE "Option E" dciS' dclS' tpr


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
      Benchmarks
-}

listInput :: Int -> [BS.ByteString]
listInput n =
  uncurry mkTestInput <$> Prelude.replicate n (1 :: Int, "0" :: Text)

makeBench
  :: (NFData a, NFData b) => (a -> b) -> String -> a -> String -> Benchmark
makeBench f fn i ipts =
  env (evaluate (force i)) $ \d -> bench (fn <> ":" <> ipts) $ nf f d

makeBenches
  :: (NFData a, NFData b) => a -> String -> [(a -> b, String)] -> [Benchmark]
makeBenches i ipts = fmap (\(a, b) -> makeBench a b i ipts)

makeGroupBenches
  :: (NFData a, NFData b, Num t1)
  => (p -> t1 -> t2)
  -> (t2 -> a)
  -> String
  -> p
  -> [(a -> b, String)]
  -> [Benchmark]
makeGroupBenches lineMaker lineSplitter inputlabel n x = do
  let inputs = lineSplitter $ lineMaker n 1
  makeBenches inputs inputlabel x

experimentInputsS
  :: (NFData a)
  => (BS.ByteString -> a)
  -> Int
  -> [[(a -> BS.ByteString, String)] -> [Benchmark]]
experimentInputsS lineSplitter n = fmap
  (\(a, b, c) -> makeGroupBenches a b c n)
  [ (failLines   , lineSplitter, "all fail :: " <> show n)
  , (passLines   , lineSplitter, "all pass :: " <> show n)
  , (onePassNfail, lineSplitter, "first pass :: " <> show n)
  , (nFailOnepass, lineSplitter, "last pass :: " <> show n)
  ]

experimentInputsL
  :: (NFData a)
  => (BL.ByteString -> a)
  -> Int
  -> [[(a -> BL.ByteString, String)] -> [Benchmark]]
experimentInputsL lineSplitter n = fmap
  (\(a, b, c) -> makeGroupBenches a b c n)
  [ (failLinesL   , lineSplitter, "all fail ::  " <> show n)
  , (passLinesL   , lineSplitter, "all pass :: " <> show n)
  , (onePassNfailL, lineSplitter, "first pass :: " <> show n)
  , (nFailOnepassL, lineSplitter, "last pass :: " <> show n)
  ]


processorsS :: Foldable f => [(f BS.ByteString -> BS.ByteString, String)]
processorsS =
  [ (L.fold (processGroup_OptionA dclS' tpr), "option A :: strict parse :: ")
  , (L.fold (processGroup_OptionB dclS' tpr), "option B :: strict parse :: ")
  -- , (L.fold (processGroup_OptionA dclS tpr) , "option A :: lazy parse :: ")
  -- , (L.fold (processGroup_OptionB dclS tpr) , "option B :: lazy parse :: ")
  ]


processorsL :: Foldable f => [(f BL.ByteString -> BL.ByteString, String)]
processorsL =
  [ (L.fold (processGroup_OptionA dclL' tpr), "option A :: strict parse :: ")
  , (L.fold (processGroup_OptionB dclL' tpr), "option B  :: strict parse :: ")
  , (L.fold (processGroup_OptionA dclL tpr) , "option A :: lazy parse :: ")
  , (L.fold (processGroup_OptionB dclL tpr) , "option B  :: lazy parse :: ")
  ]

linesAccS :: BS.ByteString -> Acc BS.ByteString
linesAccS = fromList . BS.lines

linesAccL :: BL.ByteString -> Acc BL.ByteString
linesAccL = fromList . BL.lines

linesSeqS :: BS.ByteString -> Seq BS.ByteString
linesSeqS = fromList . BS.lines

linesSeqL :: BL.ByteString -> Seq BL.ByteString
linesSeqL = fromList . BL.lines

runGroupExperiment1 :: Int -> [Benchmark]
runGroupExperiment1 n =
  fmap (\ex -> bgroup "strict bytestring :: list :: " $ ex processorsS)
       (experimentInputsS BS.lines n)
    ++ fmap (\ex -> bgroup "strict bytestring :: NA :: " $ 
          ex [(processGroup_OptionC dclS' tpr, "option C :: strict parse ")])
      (experimentInputsS id n)
    -- ++ fmap (\ex -> bgroup "lazy bytestring :: list :: " $ ex processorsL)
    --         (experimentInputsL BL.lines n)
    -- ++ fmap (\ex -> bgroup "strict bytestring :: acc :: " $ ex processorsS)
    --         (experimentInputsS linesAccS n)
    -- -- ++ fmap (\ex -> bgroup "lazy bytestring :: acc :: " $ ex processorsL)
    -- --         (experimentInputsL linesAccL n)
    -- ++ fmap (\ex -> bgroup "strict bytestring :: seq :: " $ ex processorsS)
    --         (experimentInputsS linesSeqS n)
    -- ++ fmap (\ex -> bgroup "lazy bytestring :: seq :: " $ ex processorsL)
    --         (experimentInputsL linesSeqL n)

runGroupExperiment2 :: Int -> [Benchmark]
runGroupExperiment2 n =
  fmap (\ex -> bgroup "strict bytestring :: list :: " $ ex processorsS)
       (experimentInputsS BS.lines n) 
  ++ fmap (\ex -> bgroup "strict bytestring :: NA :: " $ 
          ex [
            (processGroup_OptionC dclS' tpr, "option C :: strict parse ")
            , (processGroup_OptionD dclS' tpr, "option D :: strict parse ")])
      (experimentInputsS id n)
    -- ++ fmap (\ex -> bgroup "strict bytestring :: acc :: " $ ex processorsS)
    --         (experimentInputsS linesAccS n)
    -- ++ fmap (\ex -> bgroup "strict bytestring :: seq :: " $ ex processorsS)
    --         (experimentInputsS linesSeqS n)


makeAppBenchInput f m n = mkGroupLines $ Prelude.replicate n (f, m)

makeAppBenchInputs =
  [ ("all-pass"  , makeAppBenchInput passLines)
  , ("all-fail"  , makeAppBenchInput failLines)
  , ("first-pass", makeAppBenchInput onePassNfail)
  , ("last-pass" , makeAppBenchInput nFailOnepass)
  ]

appBenchCounts =
  [(100000, 10), (10000, 100), (1000, 1000), (100, 10000), (10, 100000)]

-- appBenchCounts = [(10000, 10), (1000, 100), (100, 1000), (10, 10000)]

-- appBenchCounts =
--     [ 
--       (1000, 10)
--     , (100, 100)
--     , (10, 1000)
--     , (1, 10000)
--     ]

cartProd x y = (,) <$> x <*> y

appBenchInputs :: [(String, BS.ByteString)]
appBenchInputs = fmap
  (\((m, n), (s, f)) -> (show n <> "groups-" <> show m <> "lines-" <> s, f m n))
  (cartProd appBenchCounts makeAppBenchInputs)

app_optionA = L.fold (processLinesApp_OptionA dciS' dclS' tpr)
-- app_optionB = L.foldM (processLinesApp_OptionB BS.putStr dciS' dclS' tpr)
app_optionC = runProcessLinesApp_OptionC dciS' dclS' tpr
app_optionD = L.fold (processLinesApp_OptionD dciS' dclS' tpr)
app_optionE = L.fold (processLinesApp_OptionE dciS' dclS' tpr)

runAppExperiment1 = fmap
  (\((inputLabel, input), (fLabel, f)) -> makeBench f fLabel input inputLabel)
  (cartProd
    appBenchInputs
    [ ("app_optionA", app_optionA . BS.lines)
    -- , ("app_optionB", app_optionB . BS.lines)
    , ("app_optionC", app_optionC)
    , ("app_optionD", app_optionD . BS.lines)
    , ("app_optionE", app_optionE . BS.lines)
    ]
  )



benches =
  bgroup "group experiments" (
          -- Prelude.concatMap runGroupExperiment1 [10, 100, 1000]
      --  ++ 
       Prelude.concatMap runGroupExperiment2 [10000, 100000]) 
       : []
  --      : 
  -- [bgroup "app experiments" runAppExperiment1]

-- These files were created in a ghci session for benchmarking externally,
-- using hyperfine.
-- file1 = BS.writeFile "lineFilter-test/100000groups-10lines.jsonl" (appBenchInputs 100000 10)
-- file2 = BS.writeFile "lineFilter-test/10000groups-100lines.jsonl" (appBenchInputs 10000 100)
-- file3 = BS.writeFile "lineFilter-test/1000groups-1000lines.jsonl" (appBenchInputs 1000 1000)
-- file4 = BS.writeFile "lineFilter-test/100groups-10000lines.jsonl" (appBenchInputs 100 10000)
-- file5 = BS.writeFile "lineFilter-test/10groups-100000lines.jsonl" (appBenchInputs 10 100000)

-- file1 = BS.writeFile "lineFilter-test/100000groups-10lines-allfail.jsonl" (appBenchInputsFail 100000 10)
-- file2 = BS.writeFile "lineFilter-test/10000groups-100lines-allfail.jsonl" (appBenchInputsFail 10000 100)
-- file3 = BS.writeFile "lineFilter-test/1000groups-1000lines-allfail.jsonl" (appBenchInputsFail 1000 1000)
-- file4 = BS.writeFile "lineFilter-test/100groups-10000lines-allfail.jsonl" (appBenchInputsFail 100 10000)
-- file5 = BS.writeFile "lineFilter-test/10groups-100000lines-allfail.jsonl" (appBenchInputsFail 10 100000)

{-
      Tests
-}

testCases =
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



tests = testGroup
  "filter lines application"
  [
    -- testGroup "conduit app" $ makeTests conduitApp testCases
    testGroup "option A" $ makeTests (app_optionA . BS.lines) testCases
  , testGroup "option E" $ makeTests (app_optionE . BS.lines) testCases
  ]
  where makeTests f = fmap (\(n, i, r) -> testCase n $ f i @?= r)
