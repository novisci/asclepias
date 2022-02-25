{-| Test.Tasty framework for @Hygiea@. 
  -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Hygiea where

import           Data.Proxy
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
                                                , testFailed
                                                , testPassed
                                                )

import           Data.Aeson                     ( ToJSON(..)
                                                , encodeFile
                                                )
import           System.FilePath                ( replaceExtension )
import           Witch.TryFrom

  {- Test constructors -}

-- TODO


  {- Internals -}

-- | In the return type of processElems. Note @input@, @output@ typically are
-- lists of some unit type that implements @TryFrom [TestMap]@.
data ProcessedElems input output = MkProcessedElems
  { getInput  :: input
  , getOutput :: output
  }

-- | Structure wrapping a @csvFile@ path, a
-- corresponding @dhallSchema@ specifying column names
-- and types, and @elemType@, a proxy to the Haskell
-- type to which the csv should be converted by way of
-- @[TestMap]@. 
data RoutineElem a = MkRoutineElem
  { csvFile     :: String
  , dhallSchema :: String
  , elemType    :: Proxy a
  }
  deriving (Show, Eq)

-- | The test provider structure: a pair of @RoutineElem
-- input@ and @RoutineElem output@ such that @Testable
-- input output@. Note the @ToJSON@ constraint, used for golden testing. The
-- choice of output format might change. In particular, if the output is chosen
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

runGolden
  :: forall input output
   . (ToJSON input, ToJSON output, Testable input output)
  => RoutineElem input
  -> RoutineElem output
  -> IO Result
runGolden i o = do
  procData <- processElems i o
  let oExpected = getOutput procData
  let oActual   = (toOutput @input @output) $ getInput procData

  -- Write golden and output files, returning file paths
  fileExpected <- writeGoldenFile o oExpected
  fileActual   <- writeOutputFile o oActual

  -- TODO run the tests

  return $ testPassed "TODO: this is dummy output. you didn't test anything."

-- | Read csv and schema, parse schema into TestMap and attempt conversion to
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

  -- NOTE input element failure always takes precedent
  -- in most cases, [TestMap] will be converted to input/output of the form
  -- [v] for some v.
  case (iData, oData) of
    (Right ii, Right oo) ->
      case (tryFrom @[TestMap] ii, tryFrom @[TestMap] oo) of
        (Right iOut, Right oOut) -> return $ MkProcessedElems iOut oOut
        (Right _   , Left err  ) -> fail $ show $ ConversionException err
        (Left  err , _         ) -> fail $ show $ ConversionException err
    (Right _  , Left err) -> fail $ show err
    (Left  err, _       ) -> fail $ show err


  {- Utilities -}

-- TODO consider whether JSON is the best output format.
--
-- TODO we need a file organization/naming convention. For now, just dump
-- things to where they came from.

writeJSONWithExt :: (ToJSON a) => String -> RoutineElem a -> a -> IO String
writeJSONWithExt ext elem d = do
  let file = replaceExtension (csvFile elem) ext
  encodeFile file d
  return file

-- | Create a JSON golden file from @a@, writing to @RoutineElem a@ @csvFile@
-- field with extension modified to be ".golden." Returns the modified file
-- path.
writeGoldenFile :: (ToJSON a) => RoutineElem a -> a -> IO String
writeGoldenFile = writeJSONWithExt "golden"

-- | Create a JSON file from @a@, writing to @RoutineElem a@ @csvFile@ field
-- with extension modified to be ".json." Returns the modified file path.
writeOutputFile :: (ToJSON a) => RoutineElem a -> a -> IO String
writeOutputFile = writeJSONWithExt "json"
