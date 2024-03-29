{-|
   @Test.Tasty@ tree-builders for @Monarch@. The main exported type is a
   @Routine@, an existential type that holds a test configuration with a
   variant for each test framework within which the test should be run. At the
   moment, only golden tests via `tasty-silver` are supported.

   @monarchTest@ is the primary exported tree-builder, which takes a @Routine@
   and builds the a @TestTree@ using the appropriate framework.

   This module is not solely a switch between testing frameworks: Tests are
   built from user-defined csv input and associated dhall schema. The
   unexported function @processElems@ transforms these to the appropriate
   Haskell types. Csv-provided inputs are transformed to outputs via
   @ToOutput@, against which the csv-provided output is compared.
  -}

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module Test.Tasty.Monarch
  ( RoutineContext
  , TestRoutine(..)
  , RoutineElem(..)
  , monarchTest
  ) where

import           Test.Monarch.MonarchException
import           Test.Monarch.Parse
import           Test.Monarch.TestMap
import           Test.Monarch.ToOutput
import           Test.Tasty.Providers          (TestName, TestTree)

import           Test.Tasty.Silver.Internal    (GDiff (..), GShow (..))

import           Data.Aeson                    (ToJSON (..), encode, encodeFile)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8', encodeUtf8)
import           Data.Text.Encoding.Error      (UnicodeException)
import           System.FilePath               (replaceExtension)
import           Test.Tasty.Silver.Advanced    (goldenTest1)
import           Witch.TryFrom

  {- Test constructors and types -}

-- TODO rethink api here, to make it easier to use.

-- | Context alias for constraints required in testing routines.
type RoutineContext input output
  = (ToJSON input, ToJSON output, Testable input output)

-- TODO add Unit and IGolden variants

-- | A pair of @RoutineElem@ for text-based input and output, used to run
-- tests. Variants specify in which framework tests should be run.
data TestRoutine
  = forall input output
  . (RoutineContext input output) =>
    Golden (RoutineElem input) (RoutineElem output)

-- TODO this API is a likely source of user error and probably should be changed.

-- | Structure wrapping a @csvFile@ path, a
-- corresponding @dhallSchema@ specifying column names
-- and types. The type to which the csv should be converted by way of
-- @[TestMap]@ is the phantom @a@, which should be specified using
-- `TypeApplications`.
data RoutineElem a = MkRoutineElem
  { csvFile     :: String
  , dhallSchema :: String
  }
  deriving (Show, Eq)

-- | Main point of entry to for tools from @Test.Monarch@. Converts a @Routine@
-- specifying a test framework and text configuration file paths to a
-- @TestTree@, for inclusion in some @Test.Tasty@ runner.
monarchTest :: TestName -> TestRoutine -> TestTree
monarchTest name (Golden rin rout) = runGolden name rin rout

  {- Internals -}

-- | In the return type of processElems. Note @input@, @output@ typically are
-- lists of some unit type that implements @TryFrom [TestMap]@. Carries with it
-- the Csv file names from which inputs/outputs were built.
data ProcessedElems input output = MkProcessedElems
  { getInput  :: input
  , inputCsv  :: String
  , getOutput :: output
  , outputCsv :: String
  }



  {- Framework-specific tree-builders -}
  -- TODO change these from result-builders to tree-builders

  -- GOLDEN

-- TODO consider whether JSON is the best output format.
-- TODO we need a file organization/naming convention. For now, just dump
-- things to where they came from.

-- TODO ASAP for some reason golden file does not get produced for inspection

-- | Unexported tree-builder for golden tests, via `tasty-silver`. Compare with
-- `goldenVsFile`.
runGolden
  :: forall input output
   . (RoutineContext input output)
  => TestName
  -> RoutineElem input
  -> RoutineElem output
  -> TestTree
runGolden name i o = goldenTest1 name
                                 expected
                                 actual
                                 textLikeDiff
                                 textLikeShow
                                 update
 where
  procData = processElems i o
  -- NOTE: failures in generating procData are thrown in processElems,
  -- which gives richer info than if 'Nothing' were provided for expected
  expected = procData >>= fmap Just . runGoldenExpected
  actual   = procData >>= runGoldenActual
  -- TODO we're doing this twice. might want to just do it in processElems
  path     = replaceExtension (csvFile o) "golden"
  -- NOTE comments about UTF-8
  -- https://hackage.haskell.org/package/text-2.0/docs/Data-Text-IO.html
  update   = BS.writeFile path . encodeUtf8

-- | Convert the expected output in @getOutput@ to @Text@, creating the actual
-- `.golden` file in the process. Writing out the file is not necessary to run
-- the test, but it is useful in allowing later inspection.
runGoldenExpected
  :: forall input output
   . (RoutineContext input output)
  => ProcessedElems input output
  -> IO T.Text
runGoldenExpected procData = do
  let txt = encodeText $ getOutput procData
  -- NOTE converting to text before writing
  case txt of
    Right txt' -> txt' <$ writeGoldenFile (outputCsv procData) txt'
    Left  err  -> fail $ show err

-- | Convert processed @input@ to @output@ via @toOutput@ and encode as JSON. Note the actual @getOutput@ data field from the @ProcessedElems@ is not used here: Only the type @output@ matters, used in the conversion. See @runGoldenExpected@.
runGoldenActual
  :: forall input output
   . (RoutineContext input output)
  => ProcessedElems input output
  -> IO T.Text
runGoldenActual procData =
  let oActual = (toOutput @input @output) $ getInput procData
  in  case encodeText oActual of
        Right txt -> pure txt
        Left  err -> fail $ show err

-- | Read csv and schema, parse schema into [TestMap] and attempt conversion to
-- input and output types. Fails if any step of the process returns an
-- exception. This is the first step of running Monarch tests using any
-- framework option.
processElems
  :: (Testable input output)
  => RoutineElem input
  -> RoutineElem output
  -> IO (ProcessedElems input output)
processElems i o = do
  -- read schema
  iSchema <- parseDhallFile $ dhallSchema i
  oSchema <- parseDhallFile $ dhallSchema o
  -- build decoders
  let iDecoder = decodeMapSchemaAuto @TestVal iSchema
  let oDecoder = decodeMapSchemaAuto @TestVal oSchema
  let iFile    = csvFile i
  let oFile    = csvFile o
  -- parse Csv with decoder into [TestMap]
  iData <- tryParseRecordsCsv iDecoder iFile
  oData <- tryParseRecordsCsv oDecoder oFile


  -- Convert from [TestMap]
  -- NOTE input element failure always takes precedent
  -- in most cases, [TestMap] will be converted to input/output of the form
  -- [v] for some v.
  case (iData, oData) of
    (Right ii, Right oo) ->
      case (tryFrom @[TestMap] ii, tryFrom @[TestMap] oo) of
        (Right iOut, Right oOut) ->
          pure $ MkProcessedElems iOut (csvFile i) oOut (csvFile o)
        (Right _, Left err) ->
          fail $ show $ OutputConversionException err oFile
        (Left err, _) -> fail $ show $ InputConversionException err iFile
    (Right _  , Left err) -> fail $ show err
    (Left  err, _       ) -> fail $ show err


  {- Utilities -}

-- Write JSON files, or encode as text
encodeText :: (ToJSON a) => a -> Either UnicodeException T.Text
encodeText = decodeUtf8' . B.toStrict . encode

-- returns file path, for convenience
writeJSONWithExt :: (ToJSON a) => String -> String -> a -> IO ()
writeJSONWithExt ext path d = do
  let file = replaceExtension path ext
  encodeFile file d

-- | Create a JSON golden file from @a@, writing to @path@ with extension
-- modified to be ".golden."
writeGoldenFile :: (ToJSON a) => String -> a -> IO ()
writeGoldenFile = writeJSONWithExt "golden"

-- | Create a JSON golden file from @a@, writing to @path@ with extension
-- modified to be ".json."
writeOutputFile :: (ToJSON a) => String -> a -> IO ()
writeOutputFile = writeJSONWithExt "json"

-- copy-paste from tasty-silver
textLikeShow :: T.Text -> GShow
textLikeShow = ShowText

textLikeDiff :: T.Text -> T.Text -> GDiff
textLikeDiff x y | x == y    = Equal
                 | otherwise = DiffText Nothing x y
