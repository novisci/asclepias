{-|
Module      : Hasklepias.MakeCohortApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasklepias.MakeCohortApp
  ( CohortApp(..)
  , makeCohortApp
  , shapeOutput
  , runApp
  , runAppWithLocation
  ) where


import           Cohort
import           Colog.Core                     ( (<&)
                                                , HasLog(..)
                                                , LogAction(..)
                                                , logPrint
                                                , logPrintStderr
                                                , logStringStderr
                                                , logStringStdout
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                , decode
                                                , encode
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
import qualified Data.ByteString.Lazy          as BL
import           Data.List                      ( sort )
import           Data.Map.Strict                ( fromList
                                                , toList
                                                )
import qualified Data.Map.Strict               as M
                                                ( fromListWith
                                                , toList
                                                )
import           Data.Monoid                    ( Monoid(mconcat) )
import           Data.String.Interpolate        ( i )
import           Data.Text                      ( Text
                                                , pack
                                                , splitOn
                                                )
import           Development.GitRev             ( gitDirty
                                                , gitHash
                                                )
import           EventDataTheory         hiding ( (<|>) )
import           Hasklepias.AppUtilities
import           Options.Applicative
import           Options.Applicative.Help
                                         hiding ( fullDesc )
import           Type.Reflection                ( Typeable )
import           Witch                          ( into )

{-| INTERNAL
A type which contains the evaluation options of a cohort application.
These options are set at the command line.
-}
data MakeCohort = MakeCohort
  { -- | Tells the application defined by 'makeCohortApp' where get @'Input'@.
    input               :: !Input
    -- | Tells the application defined by 'makeCohortApp' where send @'Output'@.
  , output              :: !Output
    -- | Sets the 'SubjectSample' for cohort evaluation.
    --   This option gives users the ability to filter the population
    --   to a particular set of subjects for testing.
  , subjectSampleOpt    :: !SubjectSample
    -- | Sets the 'EvaluateFeatures' option for cohort evaluation.
  , evaluateFeaturesOpt :: !EvaluateFeatures
    -- | Decompress gzipped input
  , inDecompress        :: InputDecompression
    -- | Compress output using gzip
  , outCompress         :: OutputCompression
  }

{-| INTERNAL
The @Parser@ for @MakeCohort@ command line options
-}
makeCohortParser :: Parser MakeCohort
makeCohortParser =
  MakeCohort
    <$> inputParser
    <*> outputParser
    <*> subjectSampleParser
    <*> evaluateFeaturesParser
    <*> inputDecompressionParser
    <*> outputCompressionParser

ioDoc :: Doc
ioDoc = dullblue (bold "== I/O options ==") <> linebreak <> [i|
  This application can get/put data from/to a file, an S3 location, or stdin/stdout.
  I/O locations can be mixed. For example, data can be read in from a local file
  and streamed out to S3. The defaults are stdin/stdout. See Available Options
  for the command line options to set file or S3 input/output.
  |]

{-| INTERNAL
A helper function used in 'makeCohortApp' used to include
a project's name and version in the cohort application's help text.
-}
makeCohortParserInfo
  :: String -- ^ name of project
  -> String -- ^ version of project
  -> ParserInfo MakeCohort
makeCohortParserInfo name version = Options.Applicative.info
  (makeCohortParser <**> (helper <*> verisonOption))
  (fullDesc <> header (name <> " " <> versionInfo) <> progDescDoc
    (Just
      ([i| 
  Create cohorts for #{ name } 
  based on code from gitrev: #{ githash }.
  
  #{ gitdirty }
  |]
      <> helpText
      )
    )
  )
 where
  gitinfo  = [i| (gitrev: #{githash})|]
  githash  = pack $(gitHash)
  dirtygit = $(gitDirty)
  gitdirty = if dirtygit
    then
      yellow
        "**There were uncommitted files in the project repo when this application was built.**"
    else ""
  versionInfo = version <> " " <> gitinfo
  verisonOption =
    infoOption versionInfo (long "version" <> help "Show version")
  helpText =
    line
      <> ioDoc
      <> line
      <> line
      <> subjectSampleDoc
      <> line
      <> line
      <> evaluateFeaturesDoc
      <> line

{-
Defines the @Parser@ for @'SubjectSample'@ command line options.
-}
subjectSampleParser :: Parser SubjectSample
subjectSampleParser =
  (SubjectIncludeList . splitOn "|" <$> strOption
      (long "keep-subjects" <> metavar "KEEPIDS" <> help
        (  "'|' separated list of subject IDs "
        <> "to keep from the population. "
        <> "This option should only be used for testing."
        )
      )
    )
    <|> (SubjectExludeList . splitOn "|" <$> strOption
          (long "drop-subjects" <> metavar "DROPIDS" <> help
            (  "'|' separated list of subject IDs "
            <> "to drop from the population. "
            <> "This option should only be used for testing."
            )
          )
        )
    <|> (FirstNSubjects <$> option
          auto
          (long "first-n-subjects" <> metavar "N" <> help
            (  "Process only the first N subjects in the population. "
            <> "This option should only be used for testing."
            )
          )
        )
    <|> pure AllSubjects

subjectSampleDoc :: Doc
subjectSampleDoc =
  dullblue (bold "== Filter Population Options ==")
    <> linebreak
    <> dullyellow
         (bold "These options are meant for testing and debugging purposes.")
    <> linebreak
    <> dullred (underline (bold "Do not use in production."))
    <> line
    <> line
    <> [i|
  By default, all subjects in the input population are evaluated for cohort
  inclusion. Several options are available to filter the population to 
  particular subjects. For example, the --first-n-subjects option processes
  the first N subjects in the input data. This can be useful to "kick the tires"
  of the cohort application and limit the amount of data to process.
  |]

{-
Defines the @Parser@ for @'EvaluateFeatures'@ command line options.
-}
evaluateFeaturesParser :: Parser EvaluateFeatures
evaluateFeaturesParser =
  flag'
      SkipFeatures
      (  long "skip-features"
      <> help
           "Skip evaluating any features. This can be used to evaluate a cohort just for attrition info."
      )
    <|> flag'
          OnAll
          (  long "features-on-all-units"
          <> help
               "Evaluate features on all observational units, regardless of their inclusion status."
          )
    <|> pure OnlyOnIncluded

evaluateFeaturesDoc :: Doc
evaluateFeaturesDoc =
  dullblue (bold "== Feature Evalution Options ==") <> linebreak <> [i|
  By default, features defined in the cohort are only evaluated for 
  observational units included in the cohort. The application has two 
  option flags to change this behavior: skip-features and features-on-all-units.
  See Available options. 
  |]

{-| INTERNAL
TODO
-}
mapIntoPop
  :: forall m t a
   . (Ord a, Ord t, Eq m)
  => [(SubjectID, Event t m a)]
  -> Population [Event t m a]
mapIntoPop l = into $ fmap
  -- TODO: is there a way to avoid the sort?
  (\(id, es) -> into @(Subject [Event t m a]) (into @Text id, sort es))
  (collectBySubject l)
 where
  collectBySubject x = M.toList $ M.fromListWith (++) (fmap (fmap pure) x)


{-| INTERNAL
Creates a cohort builder function
-}
makeCohortBuilder
  :: ( Eventable t m a
     , EventLineAble t m a b
     , FromJSONEvent t m a
     , ToJSON d0
     , ShapeCohort d0 i
     , Monad f
     )
  => CohortEvalOptions
  -> CohortMapSpec [Event t m a] d0 i
  -> BL.ByteString
  -> f ([LineParseError], CohortMap d0 i)
makeCohortBuilder opts specs x = do
  let doEvaluation = makeCohortSpecsEvaluator opts specs
  let dat = second mapIntoPop $ parseEventLinesL defaultParseEventLineOption x
  pure $ doEvaluation =<< dat

reshapeCohortMap
  :: (Cohort d0 i -> CohortJSON) -> CohortMap d0 i -> CohortMapJSON
reshapeCohortMap g x =
  MkCohortMapJSON $ fromList $ fmap (fmap g) (toList $ into x)

shapeOutput
  :: (Monad m, ShapeCohort d0 i)
  => (Cohort d0 i -> CohortJSON)
  -> m ([LineParseError], CohortMap d0 i)
  -> m ([LineParseError], CohortMapJSON)
shapeOutput shape = fmap (fmap (reshapeCohortMap shape))

-- logging based on example here:
-- https://github.com/kowainik/co-log/blob/main/co-log/tutorials/Main.hs
parseErrorL :: LogAction IO LineParseError
parseErrorL = logPrintStderr

logParseErrors :: [LineParseError] -> IO ()
logParseErrors x = mconcat $ fmap (parseErrorL <&) x

{-|
Type containing a cohort app.
The @Maybe Location@ argument can be used to set a location of input data
for (e.g.) usage in tests.
The return type contains the `Output` location so that the application 
captures the output location from the cli arguments,
but can also be overridden by (e.g.) `runAppWithLocation`.
-}
newtype CohortApp m = MkCohortApp { runCohortApp :: Maybe Location -> m (BL.ByteString, (Output, OutputCompression)) }

-- | Make a command line cohort building application.
makeCohortApp
  :: ( Eventable t m a
     , EventLineAble t m a b
     , FromJSONEvent t m a
     , ToJSON d0
     , ShapeCohort d0 i
     )
  => String  -- ^ cohort name
  -> String  -- ^ app version
  -> (Cohort d0 i -> CohortJSON) -- ^ a function which specifies the output shape
  -> CohortMapSpec [Event t m a] d0 i  -- ^ a list of cohort specifications
  -> CohortApp IO
makeCohortApp name version shape spec = MkCohortApp $ \l -> do
  options <- execParser (makeCohortParserInfo name version)
  let errLog = logStringStderr

  errLog <& "Creating cohort builder..."
  let cohortEvalOpts = MkCohortEvalOptions (evaluateFeaturesOpt options)
                                           (subjectSampleOpt options)
  let app = makeCohortBuilder cohortEvalOpts spec

  errLog <& "Reading data from stdin..."
  -- TODO: give error if no contents within some amount of time

  let loc = case l of
        Nothing -> inputToLocation $ input options
        Just x  -> x

  dat <- readData loc (inDecompress options)

  errLog <& "Bulding cohort..."
  res <- shapeOutput shape (app dat)

  logParseErrors (fst res)

  errLog <& "Encoding cohort(s) output and writing out..."

  pure (encode (toJSON (snd res)), (output options, outCompress options))

-- | Just run the thing.
runApp :: CohortApp IO -> IO ()
runApp x = do
  app <- runCohortApp x Nothing
  writeData (outputToLocation $ fst $ snd app) (snd $ snd app) (fst app)


-- | Just run the thing with a set location (e.g for testing).
runAppWithLocation :: Location -> CohortApp IO -> IO BL.ByteString
runAppWithLocation l x = do
  app <- runCohortApp x (Just l)
  pure $ fst app
