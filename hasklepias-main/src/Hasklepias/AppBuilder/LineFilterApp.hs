{-|
-}
{-# LANGUAGE TypeApplications #-}

module Hasklepias.AppBuilder.LineFilterApp
  ( makeLineFilterApp
  , makeFilterEventLineApp
  ) where

import qualified Control.Foldl                 as L
import           Data.Aeson                     ( decodeStrict' )
import qualified Data.ByteString.Char8         as BS
import           EventDataTheory         hiding ( (<|>) )
import           Hasklepias.AppBuilder.LineFilterApp.LineFilterLogic
import           Hasklepias.AppUtilities
import           Options.Applicative

-- Container for app options
data LineFilterAppOpts = MkLineFilterAppOpts
  { input  :: Input
  , output :: Output
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
  <$>  (fileInput <|> s3Input <|> stdInput)
  <*>  (fileOutput <|> s3Output <|> stdOutput)
  <**> helper
  )
  (fullDesc <> progDesc desc <> header ("Filter events for " <> name))

-- A function that performs the logic of line processing
type LineFilterLogic i a
  =  (BS.ByteString -> i)  -- ^ identifier parser
  -> (BS.ByteString -> Maybe a)  -- ^ parsing function
  -> (a -> Bool)  -- ^ predicate
  -> BS.ByteString
  -> BS.ByteString

{-| 
Creates a application that filters (groups of) lines based on:

* a function which identifies groups of lines
* a function which parses lines into some type `a`
* a predicate function: `a  -> Bool`

All groups of lines where
at least one line satisfies the predicate 
are output.
The rest are dropped.

-}
makeLineFilterApp
  :: (Eq a, Eq i)
  => LineFilterLogic i a
  -> String -- ^ name of the app (e.g. a project's id)
  -> (BS.ByteString -> i) -- ^ parser for line identifier
  -> (BS.ByteString -> Maybe a) -- ^ parser
  -> (a -> Bool) -- ^ predicate
  -> IO ()
makeLineFilterApp logic name pid psl prd = do
  options <- execParser (makeAppArgs name)
  let inloc  = inputToLocation $ input options
  let outloc = outputToLocation $ output options

  dat <- readDataStrict inloc

  writeDataStrict outloc $ logic pid psl prd dat

{-| 
Create a application that filters event data with two arguments: 
  * a string for the name of the application (e.g. the project ID)
  * a predicate function of type @Event c m a -> Bool@. 

The application takes event data formatted as [`ndjson`](http://ndjson.org/)
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
  => LineFilterLogic (Maybe SubjectIDLine) (Event t m a)
  -> String -- ^ name of the app (e.g. a project's id)
  -> (Event t m a -> Bool) -- ^ predicate to evaluate for each event
  -> IO ()
makeFilterEventLineApp logic name = makeLineFilterApp
  logic
  name
  (decodeStrict' @SubjectIDLine)
  (fmap snd . decodeEventStrict' defaultParseEventLineOption)
