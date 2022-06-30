module Hasklepias.AppBuilder.LineFilterApp (
   makeLineFilterAppF
   , makeLineFilterAppF'
   , makeLineFilterAppC
   , makeLineFilterAppC'
) where 

import Conduit
import Data.Aeson
import qualified Data.ByteString.Char8         as C
import Hasklepias.AppUtilities
import Hasklepias.AppBuilder.LineFilterApp.Foldl
import Hasklepias.AppBuilder.LineFilterApp.Conduit
import Options.Applicative



-- Container for app options
data LineFilterAppOpts = MkLineFilterAppOpts
  { input  :: Input
  -- TODO: sending output to a location will require a touch of reworking the 
  --       fsc function in prefilterC. Currently, this function spits to stdout
  --       as it goes, which won't work for S3 and a local file would need to be
  --       appended to.
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
  <*> (fileOutput <|> s3Output <|> stdOutput)
  <**> helper
  )
  (fullDesc <> progDesc desc <> header ("Filter events for " <> name))


-- | Type containing the filter app
newtype LineFilterApp m = MkFilterApp { runPreApp :: Maybe Location -> m C.ByteString }


{- | 
Create a application that filters event data with two arguments: 
  * a string for the name of the application (e.g. the project ID)
  * a predicate function of type @Event c m a -> Bool@. 

-}
makeLineFilterAppF
  :: (Eq a, Eq i)
  => String -- ^ name of the app (e.g. a project's id)
  -> (C.ByteString -> i)
  -> (C.ByteString -> Maybe a) -- ^ parser
  -> (a -> Bool) -- ^ Predicate
  -> IO ()
makeLineFilterAppF name pid psl prd = do
  options <- execParser (makeAppArgs name)
  let loc = inputToLocation $ input options

  dat <- readDataStrict loc
  filterAppF pid psl prd dat

makeLineFilterAppF'
  :: (Eq a, Eq i)
  => String -- ^ name of the app (e.g. a project's id)
  -> (C.ByteString -> i)
  -> (C.ByteString -> Maybe a) -- ^ parser
  -> (a -> Bool) -- ^ Predicate
  -> IO ()
makeLineFilterAppF' name pid psl prd = do
  options <- execParser (makeAppArgs name)
  let loc = inputToLocation $ input options

  dat <- readDataStrict loc
  filterAppF' pid psl prd dat

makeLineFilterAppC
  :: (Eq a, Eq i)
  => String -- ^ name of the app (e.g. a project's id)
  -> (C.ByteString -> i)
  -> (C.ByteString -> Maybe a) -- ^ parser
  -> (a -> Bool) -- ^ Predicate
  -> IO ()
makeLineFilterAppC name pid psl prd = do
  options <- execParser (makeAppArgs name)
  let loc = inputToLocation $ input options

  dat <- readDataStrict loc
  runConduit (filterAppC C.putStrLn pid psl prd dat)

makeLineFilterAppC'
  :: (Eq a, Eq i)
  => String -- ^ name of the app (e.g. a project's id)
  -> (C.ByteString -> i)
  -> (C.ByteString -> Maybe a) -- ^ parser
  -> (a -> Bool) -- ^ Predicate
  -> IO ()
makeLineFilterAppC' name pid psl prd = do
  options <- execParser (makeAppArgs name)
  let loc = inputToLocation $ input options

  dat <- readDataStrict loc
  C.putStr $ runConduitPure (filterAppC' pid psl prd dat)