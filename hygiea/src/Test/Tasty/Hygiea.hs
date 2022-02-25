{-| 
   Test.Tasty tree-builders for @Hygiea@. 
   

  -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Hygiea
  ( RoutineContext
  , Routine(..)
  , RoutineElem(..)
  , hTest
  ) where

import Data.Proxy
import           Data.Typeable                  ( Typeable )
import           Test.Hygiea.HygieaException
import           Test.Hygiea.Map
import           Test.Hygiea.Parse
import           Test.Hygiea.ToOutput
import           Test.Tasty.Options             ( IsOption(..)
                                                , OptionDescription(..)
                                                , lookupOption
                                                )
import           Test.Tasty.Providers           ( IsTest(..)
                                                , Result(..)
                                                , TestName
                                                , TestTree
                                                , testFailed
                                                , testPassed
                                                )

import           Test.Tasty.Silver.Internal     ( GDiff(..)
                                                , GShow(..)
                                                )

import           Data.Aeson                     ( ToJSON(..)
                                                , encode
                                                , encodeFile
                                                )
import           Data.ByteString.Lazy           ( toStrict )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.IO
import           System.FilePath                ( replaceExtension )
import           Test.Tasty.Silver.Advanced     ( goldenTest1 )
import           Witch.TryFrom

  {- Test constructors and types -}
-- TODO rethink api here, to make it easier to use.

-- | Context alias for constraints required in testing routines.
type RoutineContext input output
  = (ToJSON input, ToJSON output, Testable input output)

-- TODO add Unit and IGolden variants

-- | A pair of @RoutineElem@ for text-based input and output, used to run
-- tests. Variants specify in which framework tests should be run.
data Routine
  = forall input output
  . (RoutineContext input output) =>
    Golden (RoutineElem input) (RoutineElem output)

-- TODO this API is a good source of user error and probably should be changed. 

-- | Structure wrapping a @csvFile@ path, a
-- corresponding @dhallSchema@ specifying column names
-- and types, and @elemType@, a proxy to the Haskell
-- type to which the csv should be converted by way of
-- @[TestMap]@. 
data RoutineElem a = MkRoutineElem
  { csvFile     :: String
  , dhallSchema :: String
  , elemType :: Proxy a
  }
  deriving (Show, Eq)

-- | TODO bad naming
hTest :: TestName -> Routine -> TestTree
hTest name (Golden rin rout) = runGolden name rin rout

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

-- TODO TODO for some reason golden file does not get produced for inspection

-- | TODO compare with goldenVsFile from tasty-silver
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
  expected = fmap (Just . encodeText . getOutput) procData
  actual   = procData >>= runGoldenActual
  path     = replaceExtension (csvFile o) "golden"
  -- TODO any point in re-encoding as utf8 before write?
  update   = Data.Text.IO.writeFile path

-- | TODO
runGoldenActual
  :: forall input output
   . (RoutineContext input output)
  => ProcessedElems input output
  -> IO T.Text
runGoldenActual procData = do
  let oActual = (toOutput @input @output) $ getInput procData
  return $ encodeText oActual

-- | Read csv and schema, parse schema into [TestMap] and attempt conversion to
-- input and output types. Fails if any step of the process returns an
-- exception. This is the first step of running Hygiea tests using any
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
  let iDecoder = decodeMapSchemaAuto @TestAtomic iSchema
  let oDecoder = decodeMapSchemaAuto @TestAtomic oSchema
  -- parse Csv with decoder into [TestMap]
  iData <- tryParseRecordsCsv iDecoder $ csvFile i
  oData <- tryParseRecordsCsv oDecoder $ csvFile o

  -- Convert from [TestMap]
  -- NOTE input element failure always takes precedent
  -- in most cases, [TestMap] will be converted to input/output of the form
  -- [v] for some v.
  case (iData, oData) of
    (Right ii, Right oo) ->
      case (tryFrom @[TestMap] ii, tryFrom @[TestMap] oo) of
        (Right iOut, Right oOut) ->
          return $ MkProcessedElems iOut (csvFile i) oOut (csvFile o)
        (Right _  , Left err) -> fail $ show $ ConversionException err
        (Left  err, _       ) -> fail $ show $ ConversionException err
    (Right _  , Left err) -> fail $ show err
    (Left  err, _       ) -> fail $ show err


  {- Utilities -}

-- Write JSON files, or encode as text
encodeText :: (ToJSON a) => a -> T.Text
encodeText = decodeUtf8 . toStrict . encode

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


  {- OLD EXPERIMENT -}

  -- TODO this is for discussion. remove it.

  {-
-- | The test provider structure: a pair of @RoutineElem input@ and @RoutineElem output@ such that @Testable input output@. Note the @ToJSON@ constraint, used for golden testing. The choice of output format might change. In particular, if the output is chosen
-- to be csv of the same format as the expected 'ouptut' provided, then the
-- 'golden' file need not be written out and the user-provided file can serve
-- that purpose.
data Routine
  = forall input output
  . (ToJSON input, ToJSON output, Testable input output) =>
    Routine (RoutineElem input) (RoutineElem output)
  deriving Typeable


-- TODO
instance IsTest Routine where
  testOptions = return [Option (Proxy @Framework)]
  run opts routine _ = do
    let framewk = lookupOption opts :: Framework
    runRoutine framewk routine


-- TODO add IGolden variant for interactive golden test

-- | Test option controlling what kind of test we want to run. For example, the
-- @Golden@ variant will build a non-interactive golden test with
-- [tasty-silver](https://hackage.haskell.org/package/tasty-silver). At
-- present, that is the only supported variant, but the option is included with
-- the expectation that unit or property testing will be supported in the
-- future.
data Framework = Golden
  deriving (Eq, Typeable)

instance IsOption Framework where
  defaultValue = Golden
  parseValue "Golden" = Just Golden
  parseValue _        = Nothing
  optionName = return "framework"
  optionHelp = return "test framework to use, e.g. Golden"

-- TODO
runRoutine :: Framework -> Routine -> IO Result
runRoutine framewk (Routine input output) = case framewk of
  Golden -> runGolden input output

-}
