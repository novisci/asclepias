{-| TODO
-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}
module Hasklepias.AppBuilder.LineFilterApp
  ( makeLineFilterApp
  , makeFilterEventLineApp
  , runLineFilterAppSimple
  , runFilterEventLineAppSimple
  -- Re-exports
  , TaggerConfig(..)
  , inputTagMap
  ) where

import           Blammo.Logging
import           Blammo.Logging.LogSettings.Env             as LogSettingsEnv
import           Blammo.Logging.Simple
import           Control.Monad.IO.Class
import           Data.Aeson                                 (decodeStrict')
import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Lazy                       as BL
import           Data.String                                (IsString (fromString))
import           Data.String.Interpolate                    (i)
import           EventDataTheory                            hiding ((<|>))
import           Hasklepias.AppBuilder.ProcessLines.Logic
import           Hasklepias.AppBuilder.ProcessLines.Taggers
import           Hasklepias.AppUtilities
import           Options.Applicative
import           Options.Applicative.Help                   hiding (fullDesc)
import           System.Exit

-- Container for app options
data LineFilterAppOpts
  = MkLineFilterAppOpts
      { input          :: !Input
      , output         :: !Output
      , inDecompress   :: !InputDecompression
      , outCompress    :: !OutputCompression
      , partitionIndex :: !(Maybe PartitionIndex)
        -- , lazy :: Bool
      }

desc :: Doc
desc =
  [i|The application takes event data formatted as ndjson (http://ndjson.org/)
  (i.e. one event per line). The application returns the event data filtered to
  all those subjects who have at least one event satisfying the given predicate.
  Each subject's data must be grouped in contiguous chunks of lines; otherwise,
  the application may not behave as expected and will not warn or raise an error.
  Lines that fail to parse as an `Event` do not satisfy the predicate, but are not
  dropped from the output. In other words, all of a subject's data is returned in
  the same order as the input, provided that at least one line successfully parses
  into an Event c m and satisfies the predicate.
  |]
  <> line
  <> partitionIndexDoc
  <> line

-- Create the ParserInfo for a LineFilterApp
makeAppArgs :: String -> ParserInfo LineFilterAppOpts
makeAppArgs name = Options.Applicative.info
  (    MkLineFilterAppOpts
  <$>  inputParser
  <*>  outputParser
  <*>  inputDecompressionParser
  <*>  outputCompressionParser
  <*>  optional partitionIndexParser
  -- <*>  switch (long "lazy" <> short 'l' <> help "Whether to process as lazy bytestring")
  <**> helper
  )
  (fullDesc <> progDescDoc (Just $ desc <> line <> logSettingsHelpDoc)
  <> header ("Filter events for " <> name))

{-|
Creates a application that filters (groups of) lines based on:

* a function which constructs an identifier from a line
indicating which group the line belongs to
* a function which parses a line into some type @a@
* a predicate function: @a  -> Bool@

All groups of lines where
at least one line satisfies the predicate
are output.
The rest are dropped.

-}
makeLineFilterApp
  :: (Eq a, Eq i, Show i, MonadLogger m, MonadIO m)
  => String -- ^ name of the app (e.g. a project's id)
  -> (BS.ByteString -> Maybe i) -- ^ parser for line identifier
  -> (BS.ByteString -> Maybe a) -- ^ parser
  -> (a -> Bool) -- ^ predicate
  -> m ()
makeLineFilterApp name pid psl prd = do
  options <- liftIO $ execParser (makeAppArgs name)

  let iospec = parseIOSpec
                (partitionIndex options)
                (input options)
                (output options)

  case iospec of
    Left e -> do
      logError (fromString $ show e)
      liftIO $ exitWith (ExitFailure 1)
    Right spec -> do

      result <-
        liftIO $ processAppLinesStrict pid psl prd NoTransformation
          <$> readDataStrict (inputLocation spec) (inDecompress options)

      case result of
        Left lae -> do
          logError $ lineAppErrorMessage lae
          liftIO $ exitWith (ExitFailure 1)
        Right bs ->
          liftIO $ writeDataStrict (outputLocation spec) (outCompress options) bs

{-|
Creates an @IO@ action from @'makeLineFilterApp'@
-}
runLineFilterAppSimple
  :: (Eq a, Eq i, Show i)
  => String -- ^ name of the app (e.g. a project's id)
  -> (BS.ByteString -> Maybe i) -- ^ parser for line identifier
  -> (BS.ByteString -> Maybe a) -- ^ parser
  -> (a -> Bool) -- ^ predicate
  -> IO ()
runLineFilterAppSimple name pid psl prd = do
  logger <- newLogger =<< parseLogSettings
  runLoggerLoggingT logger $ makeLineFilterApp name pid psl prd

-- TODO types of make/runLineFilterApp would need to accommodate taggers if
-- those functions are kept generic.  alternatively, makeFilterEventLineApp
-- could be the entrypoint for project code actually using the LineFilterApp,
-- and the generic parent functions could be scrapped. see
-- https://gitlab.com/TargetRWE/epistats/nsstat/asclepias/-/issues/332

{-|
Create a application that filters event data with two arguments:

  * a string for the name of the application (e.g. the project ID)
  * a predicate function of type @Event c m a -> Bool@.

The application takes event data formatted as [ndjson](http://ndjson.org/)
(i.e. one event per line).
The application returns the event data
filtered to all those subjects
who have at least one event satisfying the given predicate.
Each subject's data must be grouped in contiguous chunks of lines;
otherwise,  the application may not behave as expected
and will not warn or raise an error.
Lines that fail to parse as an `Event` do not satisfy the predicate,
but are not dropped from the output.
In other words,
all of a subject's data is returned in the same order as the input,
provided that at least one line successfully parses
into an `Event` and satisfies the predicate.
-}
makeFilterEventLineApp
  :: (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a, MonadLogger f, MonadIO f)
  => String -- ^ name of the app (e.g. a project's id)
  -> (Event t m a -> Bool) -- ^ predicate to evaluate for each event
  -> TaggerConfig t c m
  -- ^ List of tagging functions producing @t@ tags, with signature @M.Map Text
  -- c -> m -> t@, along with the path to a dhall file with which to create
  -- @M.Map Text c@. Note @c@ must be @FromDhall@, which is enforced in the
  -- constructor.
  -> f ()
-- TODO https://gitlab.com/TargetRWE/epistats/nsstat/asclepias/-/issues/331
makeFilterEventLineApp name es _ =
  makeLineFilterApp
            name
            (decodeStrict' @SubjectID)
            (fmap snd . decodeEventStrict' defaultParseEventLineOption)
            es


{-|
Creates an @IO@ action from @'makeFilterEventLineApp'@.
-}
runFilterEventLineAppSimple
  :: (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a)
  => String -- ^ name of the app (e.g. a project's id)
  -> (Event t m a -> Bool) -- ^ predicate to evaluate for each event
  -> IO ()
runFilterEventLineAppSimple name prd = do
  logger <- newLogger =<< parseLogSettings
  -- TODO https://gitlab.com/TargetRWE/epistats/nsstat/asclepias/-/issues/331
  runLoggerLoggingT logger $ makeFilterEventLineApp name prd emptyTaggerConfig
