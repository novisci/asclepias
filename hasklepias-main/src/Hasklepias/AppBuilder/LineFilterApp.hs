{-|
-}
{-# LANGUAGE TypeApplications #-}

module Hasklepias.AppBuilder.LineFilterApp
  ( makeLineFilterApp
  , makeFilterEventLineApp
  ) where

import           Colog.Core                     ( (<&)
                                                , logStringStderr
                                                )
import           Data.Aeson                     ( decodeStrict' )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           EventDataTheory         hiding ( (<|>) )
import           Hasklepias.AppBuilder.ProcessLines.Logic
import           Hasklepias.AppUtilities
import           Options.Applicative
import           System.Exit

-- Container for app options
data LineFilterAppOpts = MkLineFilterAppOpts
  { input        :: Input
  , output       :: Output
  , inDecompress :: InputDecompression
  , outCompress  :: OutputCompression
  -- , lazy :: Bool
  }

desc =
  "The application takes event data formatted as ndjson (http://ndjson.org/) \
  \(i.e. one event per line). The application returns the event data filtered to \
  \all those subjects who have at least one event satisfying the given predicate. \
  \Each subject's data must be grouped in contiguous chunks of lines; otherwise, \
  \the application may not behave as expected and will not warn or raise an error. \
  \Lines that fail to parse as an `Event` do not satisfy the predicate, but are not \
  \dropped from the output. In other words, all of a subject's data is returned in \
  \the same order as the input, provided that at least one line successfully parses \
  \into an Event c m and satisfies the predicate."

-- Create the ParserInfo for a LineFilterApp 
makeAppArgs :: String -> ParserInfo LineFilterAppOpts
makeAppArgs name = Options.Applicative.info
  (    MkLineFilterAppOpts
  <$>  inputParser
  <*>  outputParser
  <*>  inputDecompressionParser
  <*>  outputCompressionParser
  -- <*>  switch (long "lazy" <> short 'l' <> help "Whether to process as lazy bytestring")
  <**> helper
  )
  (fullDesc <> progDesc desc <> header ("Filter events for " <> name))

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
  :: (Eq a, Eq i, Show i)
  => String -- ^ name of the app (e.g. a project's id)
  -> (BS.ByteString -> Maybe i) -- ^ parser for line identifier
  -> (BS.ByteString -> Maybe a) -- ^ parser
  -> (a -> Bool) -- ^ predicate
  -> IO ()
makeLineFilterApp name pid psl prd = do
  options <- execParser (makeAppArgs name)
  let inloc  = inputToLocation $ input options
      outloc = outputToLocation $ output options

  result <-
    processAppLinesStrict pid
                          psl
                          prd
                          noLineTransformStrict
      <$> readDataStrict inloc (inDecompress options) 

  case result of
    Left lae -> do
      logStringStderr <& show lae
      exitWith (ExitFailure 1)
    Right bs -> writeDataStrict outloc (outCompress options) bs

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
  :: (Eventable t m a, EventLineAble t m a b, FromJSONEvent t m a)
  => String -- ^ name of the app (e.g. a project's id)
  -> (Event t m a -> Bool) -- ^ predicate to evaluate for each event
  -> IO ()
makeFilterEventLineApp name = makeLineFilterApp
  name
  (decodeStrict' @SubjectIDLine)
  (fmap snd . decodeEventStrict' defaultParseEventLineOption)
