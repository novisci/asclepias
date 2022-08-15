{-|
-}
module CohortCollection
  ( runCollectionApp
  , getLocations
  , Location(..)
  , Input(..)
  , collectorApp
  ) where


import           Cohort.Output              (CohortMapJSON)
import           Conduit                    (foldMapMC, runConduit,
                                             runResourceT, yieldMany, (.|))
import           Data.Aeson                 (decode, encode)
import qualified Data.ByteString.Char8      as CH
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C (lines, putStrLn, toStrict)
import qualified Data.Conduit.List          as CL
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T (pack)
import           Hasklepias.AppUtilities    as H hiding (Input (..), fileInput)

import           Amazonka.Auth
import           Amazonka.S3
import           Options.Applicative
import           System.IO                  (stderr)

getCohortData :: Location -> InputDecompression -> IO (Maybe CohortMapJSON)
getCohortData x d = fmap decode (readData x d)

getLocations :: Input -> IO [Location]
getLocations (FileInput d f) = fmap (fmap Local)
                                    (fmap pre . lines <$> readFile f)
 where
  pre = case d of
    Nothing -> (<>) ""
    Just s  -> (<>) (s <> "/")
getLocations (S3Input b k) =
  fmap
      (\x -> S3 NorthVirginia b (ObjectKey $ T.pack $ CH.unpack $ C.toStrict x))
    .   C.lines
    <$> getS3Object NorthVirginia b k

-- | Type to hold input information. Either from file or from S3.
data Input =
     FileInput (Maybe FilePath) FilePath
   | S3Input BucketName ObjectKey
   deriving (Show)

-- | Run collection on a list of 'Location's
runCollectionApp :: [Location] -> InputDecompression -> IO B.ByteString
runCollectionApp fs d = do
  r <- runConduit $ yieldMany fs .| foldMapMC (`getCohortData` d)
  let x = fmap encode r
  let z = fromMaybe B.empty x
  pure z

fileInput :: Parser Input
fileInput =
  FileInput
    <$> optional
          (strOption $ long "dir" <> short 'd' <> metavar "DIRECTORY" <> help
            "optional directory"
          )
    <*> strOption
          (long "file" <> short 'f' <> metavar "INPUT" <> help "Input file")

s3input :: Parser Input
s3input =
  S3Input
    <$> strOption
          (long "bucket" <> short 'b' <> metavar "Bucket" <> help "S3 bucket")
    <*> strOption
          (long "manifest" <> short 'm' <> metavar "KEY" <> help
            "S3 manifest file"
          )

data CollectorApp = CollectorApp
  { input       :: Input
  , output      :: H.Output
  , inDeompress :: H.InputDecompression
  , outCompress :: H.OutputCompression
  }

collector :: Parser CollectorApp
collector =
  CollectorApp
    <$> (fileInput <|> s3input)
    <*> H.outputParser
    <*> H.inputDecompressionParser
    <*> H.outputCompressionParser

desc =
  "Collects cohorts run on different input data. The cohorts must be derived \
  \from the same cohort specification or results may be weird. Supports reading \
  \data from a local directory or from S3. In either case the input is a path \
  \to a file containing paths (or S3 keys) to each cohort part, where One line \
  \= one file.\
  \\n\n\
  \S3 capabilities are currently limited (e.g. AWS region is set \
  \to N. Virginia).\
  \\
  \Data can be output to stdout (default), to a file (using the -o option), or \
  \to S3 (using the --outbucket and --outkey options).\
  \"


collectorOpts :: ParserInfo CollectorApp
collectorOpts = Options.Applicative.info
  (collector <**> helper)
  (fullDesc <> progDesc desc <> header "cohort collector")

collectorApp :: IO ()
collectorApp = do
  options <- execParser collectorOpts

  let files = input options
  fs <- getLocations files

  r  <- runCollectionApp fs (inDeompress options)
  H.writeData (H.outputToLocation $ output options) (outCompress options) r
