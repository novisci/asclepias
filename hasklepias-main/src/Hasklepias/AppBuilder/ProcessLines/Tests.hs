{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{- HLINT ignore "Avoid restricted function" -}
module Hasklepias.AppBuilder.ProcessLines.Tests
  ( tests
  , benches
  ) where

import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Control.Exception              ( evaluate )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , decode
                                                , decode'
                                                , decodeStrict
                                                , decodeStrict'
                                                , encode
                                                , withArray
                                                )
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Either
import           Data.List                      ( nub )
import           Data.Maybe                     ( mapMaybe )
import           Data.String.Interpolate        ( i )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Vector                    ( (!) )
import           Hasklepias.AppBuilder.ProcessLines.Logic
import           Hasklepias.AppUtilities
import           Options.Applicative
import           Test.Tasty
import           Test.Tasty.Bench
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck   hiding ( output )

{-
      Types for testing
-}

newtype LineAppTesterID = MkLineAppTesterID Int deriving (Show, Eq)

instance FromJSON LineAppTesterID where
  parseJSON = withArray "FooID" $ \a -> do
    id <- parseJSON (a ! 0)
    pure $ MkLineAppTesterID id

instance Arbitrary LineAppTesterID where
  arbitrary = MkLineAppTesterID <$> arbitrary

newtype LineAppTester = MkLineAppTester Bool deriving (Show, Eq, Ord)

instance FromJSON LineAppTester where
  parseJSON = withArray "Foo" $ \a -> do
    id <- parseJSON (a ! 1)
    pure $ MkLineAppTester id

instance Arbitrary LineAppTester where
  arbitrary = MkLineAppTester <$> arbitrary

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
      App for demoing the line filter
-}

data TestAppOpts = MkTestAppOpts
  { input  :: Input
  , output :: Output
  }




{-
      Test values constructors
-}

mkTestInput :: Int -> T.Text -> BS.ByteString
mkTestInput y x = [i|[#{ show y }, #{ x }]|]

mkTestInputL :: Int -> T.Text -> BL.ByteString
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

TODO: 
rewrite the case so that test cases for each string type
can be generated by one function, 
rather than copy/pasted and modifying (e.g.) passLines to passLinesL.
-}

appTestCasesStrict =
  [ ("no input"                  , ""                   , "")
  , ("without newline at end"    , "[1,false]\n[1,true]", "[1,false]\n[1,true]")
  , ("1 group - 1 passing line"  , passLines 1 1        , passLines 1 1)
  , ("1 group -  2 passing lines", passLines 2 1        , passLines 2 1)
  , ("1 group - 10 passing lines", passLines 10 1       , passLines 10 1)
  , ( "1 group - 1 passing line - 1 bad line"
    , BS.concat [passLines 1 1, badLines 1 1]
    , BS.concat [passLines 1 1, badLines 1 1]
    )
  , ("1 group - 1 failing line"  , failLines 1 1 , "")
  , ("1 group - 2 failing lines" , failLines 2 1 , "")
  , ("1 group - 10 failing lines", failLines 10 1, "")
  , ( "1 group - 1 failing lines 1 pass line"
    , mkGroupLines [(nFailOnepass, 1)]
    , mkGroupLines [(nFailOnepass, 1)]
    )
  , ( "1 group - 2 failing lines 1 pass line"
    , mkGroupLines [(nFailOnepass, 2)]
    , mkGroupLines [(nFailOnepass, 2)]
    )
  , ( "1 group - 10 failing lines 1 pass line"
    , mkGroupLines [(nFailOnepass, 10)]
    , mkGroupLines [(nFailOnepass, 10)]
    )
  , ( "1 group - 1 pass line 1 fail lines"
    , mkGroupLines [(onePassNfail, 1)]
    , mkGroupLines [(onePassNfail, 1)]
    )
  , ( "1 group - 1 pass line 2 fail lines"
    , mkGroupLines [(onePassNfail, 2)]
    , mkGroupLines [(onePassNfail, 2)]
    )
  , ( "1 group - 1 pass line 10 fail lines"
    , mkGroupLines [(onePassNfail, 10)]
    , mkGroupLines [(onePassNfail, 10)]
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
  , ( "2 groups - 0 pass group"
    , mkGroupLines [(failLines, 10), (failLines, 10)]
    , ""
    )
  ]

appTestCasesLazy =
  [ ("no input"                  , ""                   , "")
  , ("without newline at end"    , "[1,false]\n[1,true]", "[1,false]\n[1,true]")
  , ("1 group - 1 passing line"  , passLinesL 1 1       , passLinesL 1 1)
  , ("1 group - 2 passing lines" , passLinesL 2 1       , passLinesL 2 1)
  , ("1 group - 10 passing lines", passLinesL 10 1      , passLinesL 10 1)
  , ( "1 group - 1 passing line - 1 bad line"
    , BL.concat [passLinesL 1 1, badLinesL 1 1]
    , BL.concat [passLinesL 1 1, badLinesL 1 1]
    )
  , ("1 group - 1 failing line"  , failLinesL 1 1 , "")
  , ("1 group - 2 failing lines" , failLinesL 2 1 , "")
  , ("1 group - 10 failing lines", failLinesL 10 1, "")
  , ( "1 group - 1 failing lines 1 pass line"
    , mkGroupLinesL [(nFailOnepassL, 1)]
    , mkGroupLinesL [(nFailOnepassL, 1)]
    )
  , ( "1 group - 2 failing lines 1 pass line"
    , mkGroupLinesL [(nFailOnepassL, 2)]
    , mkGroupLinesL [(nFailOnepassL, 2)]
    )
  , ( "1 group - 10 failing lines 1 pass line"
    , mkGroupLinesL [(nFailOnepassL, 10)]
    , mkGroupLinesL [(nFailOnepassL, 10)]
    )
  , ( "1 group - 1 pass line 1 fail lines"
    , mkGroupLinesL [(onePassNfailL, 1)]
    , mkGroupLinesL [(onePassNfailL, 1)]
    )
  , ( "1 group - 1 pass line 2 fail lines"
    , mkGroupLinesL [(onePassNfailL, 2)]
    , mkGroupLinesL [(onePassNfailL, 2)]
    )
  , ( "1 group - 1 pass line 10 fail lines"
    , mkGroupLinesL [(onePassNfailL, 10)]
    , mkGroupLinesL [(onePassNfailL, 10)]
    )
  , ( "2 groups - 2 pass groups"
    , mkGroupLinesL [(nFailOnepassL, 10)]
    , mkGroupLinesL [(nFailOnepassL, 10)]
    )
  , ( "2 groups - 1 pass group"
    , mkGroupLinesL [(nFailOnepassL, 10), (failLinesL, 10)]
    , nFailOnepassL 10 1
    )
  , ( "2 groups - 1 pass group"
    , mkGroupLinesL [(failLinesL, 10), (nFailOnepassL, 10)]
    , nFailOnepassL 10 2
    )
  , ( "2 groups - 0 pass group"
    , mkGroupLinesL [(failLinesL, 10), (failLinesL, 10)]
    , ""
    )
  ]

{-
Provides a way to produce a bytestring from generated test inputs.
This is full of kludge at this point
in part due to how the test input functions above are defined.
This could be cleaned/generalized in the future.
-}
makeAppInputs :: [(LineAppTesterID, [LineAppTester])] -> BS.ByteString
makeAppInputs x = BS.intercalate "\n" (fmap f x)
 where
  f (MkLineAppTesterID i, xs) = mkTestLines $ fmap
    (\(MkLineAppTester z) -> (i, T.decodeUtf8 $ BL.toStrict $ encode z))
    xs

{-
This property checks that:
the count of unique group IDs in the input where at least one line
satisfies the predicate is equal to the count of group IDs obtains 
*after* running the inputs through the processAppLines function.
-}
prop_nGroups :: [(LineAppTesterID, [LineAppTester])] -> Property
prop_nGroups x = do
  let naiveN =
        length $ nub $ fst <$> filter (\(i, lines) -> or (fmap tpr lines)) x
  let appOutput = processAppLinesStrict dciS' dclS' tpr (makeAppInputs x)
  let appN = length $ nub $ mapMaybe dciS' (BS.lines (fromRight "" appOutput))

  naiveN === appN

{-
      Tests
-}

tests = testGroup
  "line processing logic"
  [ testGroup
      "processing lines for application"
      [ testCase "identifier failure caught"
      $   show (processAppLinesStrict dciS' dclS' tpr "[1, true]\n[bad]")
      @?= "Left Line 2: failed to decode identifier"
      , testCase "identifier failure caught"
      $ show (processAppLinesStrict dciS' dclS' tpr "[1, \"bad\"]\n[1, false]")
      @?= "Left Line 1: failed to decode line"
      , testGroup "processAppLinesStrict"
        $ makeTests (processAppLinesStrict dciS' dclS' tpr) appTestCasesStrict
      , testGroup "processAppLinesLazy"
        $ makeTests (processAppLinesLazy dciL' dclL' tpr) appTestCasesLazy
      , testProperty
        "number of groups determined by processAppLines is same as naive implementation"
        prop_nGroups
      ]
  ]
 where
  makeTests f = fmap
    (\(n, i, r) -> case f i of
      Left _ ->
        testCase n $ assertFailure "Boom! this failed and shouldn't have"
      Right a -> testCase n $ assertEqual "These should be equal" a r
    )
  readOne x | x == "1"  = Just 1
            | x == "2"  = Just 2
            | otherwise = Nothing

{-
    Benchmarks
-}

makeAppBenchInputStrict f m n = mkGroupLines $ Prelude.replicate n (f, m)

makeAppBenchInputsStrict =
  [ ("all-pass"  , makeAppBenchInputStrict passLines)
  , ("all-fail"  , makeAppBenchInputStrict failLines)
  , ("first-pass", makeAppBenchInputStrict onePassNfail)
  , ("last-pass" , makeAppBenchInputStrict nFailOnepass)
  ]

makeAppBenchInputLazy f m n = mkGroupLinesL $ Prelude.replicate n (f, m)

makeAppBenchInputsLazy =
  [ ("all-pass"  , makeAppBenchInputLazy passLinesL)
  , ("all-fail"  , makeAppBenchInputLazy failLinesL)
  , ("first-pass", makeAppBenchInputLazy onePassNfailL)
  , ("last-pass" , makeAppBenchInputLazy nFailOnepassL)
  ]

-- appBenchCounts = [(10000, 10), (1000, 100), (100, 1000), (10, 10000)]

appBenchCounts = [(1000, 1), (100, 10), (10, 100), (1, 1000)]


cartProd x y = (,) <$> x <*> y

appBenchInputsStrict :: [(String, BS.ByteString)]
appBenchInputsStrict = fmap
  (\((m, n), (s, f)) -> (show n <> "groups-" <> show m <> "lines-" <> s, f m n))
  (cartProd appBenchCounts makeAppBenchInputsStrict)

appBenchInputsLazy :: [(String, BL.ByteString)]
appBenchInputsLazy = fmap
  (\((m, n), (s, f)) -> (show n <> "groups-" <> show m <> "lines-" <> s, f m n))
  (cartProd appBenchCounts makeAppBenchInputsLazy)


makeBench
  :: (NFData a, NFData b) => (a -> b) -> String -> a -> String -> Benchmark
makeBench f fn i ipts =
  env (evaluate (force i)) $ \d -> bench (fn <> ":" <> ipts) $ nf f d

runAppExperimentStrict = fmap
  (\((inputLabel, input), (fLabel, f)) -> makeBench f fLabel input inputLabel)
  (cartProd appBenchInputsStrict
            [("", fromRight "" . processAppLinesStrict dciS' dclS' tpr)]
  )

runAppExperimentLazy = fmap
  (\((inputLabel, input), (fLabel, f)) -> makeBench f fLabel input inputLabel)
  (cartProd appBenchInputsLazy
            [("", fromRight "" . processAppLinesLazy dciL' dclL' tpr)]
  )

benches = bgroup
  "line processing benchmarks"
  [ bgroup "strict bytestring" runAppExperimentStrict
  , bgroup "lazy bytestring"   runAppExperimentLazy
  ]
