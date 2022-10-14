{-|
Module      : AppUtilities
Description : Misc types and functions useful in creating Hasklepias applications.
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeApplications      #-}

module Hasklepias.AppUtilities
  (
  -- * Types and functions for handling I/O
    Location(..)
  , showLocation
  , Input
  , Output
  , readData
  , readDataStrict
  , writeData
  , writeDataStrict
  , getS3Object
  , parseIOSpec
  , outputToLocation
  , IOSpec
  , outputLocation
  , inputLocation

  -- ** Compression handling
  , InputDecompression(..)
  , OutputCompression(..)

  -- ** CLI option parsers
  , inputParser
  , stdInputParser
  , fileInputParser
  , s3InputParser
  , outputParser
  , stdOutputParser
  , fileOutputParser
  , s3OutputParser
  , inputDecompressionParser
  , outputCompressionParser

  -- ** Environmental variable parsers and utilities
  , logSettingsParser
  , parseLogSettings
  , logSettingsHelpDoc

  -- * Processing indexed files
  , updateLocationWithPartitionIndex
  , partitionIndexParser
  , partitionIndexDoc
  , PartitionIndex
  ) where

import           Amazonka                   (LogLevel (Debug, Error), Region,
                                             ToBody (..), discover, newEnv,
                                             newLogger, runResourceT, send,
                                             sinkBody)
import           Amazonka.S3                (BucketName (..),
                                             ObjectCannedACL (ObjectCannedACL_Bucket_owner_full_control),
                                             ObjectKey (..), newGetObject,
                                             newPutObject)
import           Blammo.Logging.LogSettings
import           Codec.Compression.GZip     (CompressionLevel, compress,
                                             decompress)
import           Control.Arrow              ((&&&))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor             (bimap, first)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as BL hiding (putStrLn)
import           Data.Conduit.Binary        (sinkLbs)
import           Data.Generics.Product      (HasField (field))
import           Data.List.Split            (splitOn)
import           Data.Semigroup             (Endo (..))
import           Data.String                (IsString (fromString))
import           Data.String.Interpolate    (i)
import qualified Data.Text                  as T (Text, pack, unpack)
import qualified Data.Text.IO               as T (putStrLn)
import qualified Env
import           Formatting                 (formatToString, left)
import           Lens.Micro                 (set, (<&>), (^.))
import           Lens.Micro.Extras          (view)
import           Options.Applicative
import           Options.Applicative.Help
import           Safe                       (atMay, headMay)
import           System.IO                  (stderr)
import           Text.Read                  (readMaybe)


-- | Type representing locations that data can be read from
data Location where
  Std   ::Location
  Local ::FilePath -> Location
  S3    ::Region -> BucketName -> ObjectKey -> Location
  deriving (Show)

-- | Use to print where data is from (or to)
showLocation :: Location -> T.Text
showLocation Std                                 = "stdin/stdout"
showLocation (Local f)                           = T.pack f
showLocation (S3 r (BucketName b) (ObjectKey o)) = b <> "/" <> o

-- | Type to hold input information. Either from file or from S3.
data Input =
     StdInput
   | FileInput (Maybe FilePath) FilePath
   | S3Input  Region BucketName ObjectKey
   deriving (Show)

-- | Type to hold input information. Either from file or from S3.
data Output =
     StdOutput
   | FileOutput (Maybe FilePath) FilePath
   | S3Output  Region BucketName ObjectKey

-- | Flag for whether to decompress input
data InputDecompression = NoDecompress | Decompress deriving (Show)

-- | Flag for whether to compress output
data OutputCompression = NoCompress | Compress deriving (Show)

{-
An internal helper function to handle @InputDecompression@
for lazy Bytestrings.
-}
handleInputDecompression
  :: InputDecompression -> BL.ByteString -> BL.ByteString
handleInputDecompression d = case d of
  Decompress   -> decompress
  NoDecompress -> id

{-
An internal helper function to handle @InputDecompression@
for lazy Bytestrings.
-}
handleOutputCompression :: OutputCompression -> BL.ByteString -> BL.ByteString
handleOutputCompression d = case d of
  Compress   -> compress
  NoCompress -> id

{-
An internal helper function to handle @InputDecompression@
for strict Bytestrings.

NOTE: zlib operates on Lazy Bytestrings so this function goes
from Strict to Lazy and back to Strict.
This is likely to be inefficient.

TODO:
This could be made more efficient using the tip from the `DecompressParams` here:
https://hackage.haskell.org/package/zlib-0.6.3.0/docs/Codec-Compression-Zlib-Internal.html#g:5

    "One particular use case for setting the decompressBufferSize is
    if you know the exact size of the decompressed data
    and want to produce a strict ByteString.
    The compression and decompression functions use lazy ByteStrings
    but if you set the decompressBufferSize correctly
    then you can generate a lazy ByteString with exactly one chunk,
    which can be converted to a strict ByteString
    in O(1) time using concat . toChunks."

-}
handleInputDecompressionStrict
  :: InputDecompression -> BS.ByteString -> BS.ByteString
handleInputDecompressionStrict d = case d of
  Decompress   -> BL.toStrict . decompress . BL.fromStrict
  NoDecompress -> id

{-
An internal helper function to handle @InputDecompression@
for lazy Bytestrings.
-}
handleOutputCompressionStrict
  :: OutputCompression -> BS.ByteString -> BS.ByteString
handleOutputCompressionStrict d = case d of
  Compress   -> BL.toStrict . compress . BL.fromStrict
  NoCompress -> id

-- | Read data from a @Location@ to lazy @ByteString@
readData :: Location -> InputDecompression -> IO BL.ByteString
readData Std        d = handleInputDecompression d <$> BL.getContents
readData (Local x ) d = handleInputDecompression d <$> BL.readFile x
readData (S3 r b k) d = handleInputDecompression d <$> getS3Object r b k

-- | Write data from a @Location@ to lazy @ByteString@
writeData :: Location -> OutputCompression -> BL.ByteString -> IO ()
writeData Std        z x = BL.putStr (handleOutputCompression z x)
writeData (Local f ) z x = BL.writeFile f (handleOutputCompression z x)
writeData (S3 r b k) z x = putS3Object r b k (handleOutputCompression z x)

-- | Read data from a @Location@ to strict @ByteString@.
readDataStrict :: Location -> InputDecompression -> IO BS.ByteString
readDataStrict Std       d = handleInputDecompressionStrict d <$> BS.getContents
readDataStrict (Local x) d = handleInputDecompressionStrict d <$> BS.readFile x
readDataStrict (S3 r b k) d =
  handleInputDecompressionStrict d <$> fmap BL.toStrict (getS3Object r b k)

-- | Write data from a @Location@ to strict @ByteString@.
writeDataStrict :: Location -> OutputCompression -> BS.ByteString -> IO ()
writeDataStrict Std z x = BSC.putStrLn (handleOutputCompressionStrict z x)
writeDataStrict (Local f) z x =
  BS.writeFile f (handleOutputCompressionStrict z x)
writeDataStrict (S3 r b k) z x =
  putS3Object r b k (handleOutputCompressionStrict z x)

-- | Get an object from S3.
getS3Object :: Region -> BucketName -> ObjectKey -> IO BL.ByteString
getS3Object r b k = do
  lgr <- newLogger Debug stderr
  env <-
    newEnv discover
    <&> set (field @"envLogger") lgr
    .   set (field @"envRegion") r
  runResourceT $ do
    result <- send env (newGetObject b k)
    view (field @"body") result `sinkBody` sinkLbs

-- | Put an object on S3.
--
-- NOTE: the put request uses the bucket-owner-full-control
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html canned ACL>.
--
class (ToBody a) => PutS3 a where
  putS3Object :: Region -> BucketName -> ObjectKey -> a -> IO ()
  putS3Object r b k o = do
    lgr <- newLogger Error stderr
    env <-
      newEnv discover
      <&> set (field @"envLogger") lgr
      .   set (field @"envRegion") r
    let obj = set (field @"acl") (Just ObjectCannedACL_Bucket_owner_full_control)
              (newPutObject b k (toBody o))
    runResourceT $ do
      void . send env $ obj
      liftIO . T.putStrLn $
          "Successfully Uploaded contents to "
        <> T.pack (show b)
        <> " - "
        <> T.pack (show k)

instance PutS3 BL.ByteString
instance PutS3 BS.ByteString

-- | Maps an @Input@ to a @Location@.
inputToLocation :: Input -> Location
inputToLocation StdInput        = Std
inputToLocation (FileInput d f) = Local (pre f)
 where
  pre = case d of
    Nothing -> (<>) ""
    Just s  -> (<>) (s <> "/")
inputToLocation (S3Input r b k) = S3 r b k

-- | Maps an @Input@ to a @Location@.
outputToLocation :: Output -> Location
outputToLocation StdOutput        = Std
outputToLocation (FileOutput d f) = Local (pre f)
 where
  pre = case d of
    Nothing -> (<>) ""
    Just s  -> (<>) (s <> "/")
outputToLocation (S3Output r b k) = S3 r b k

{-
    CLI option parsers
-}

-- | Parser @StdInput@.
stdInputParser :: Parser Input
stdInputParser = pure StdInput

-- | Parser @StdOutput@.
stdOutputParser :: Parser Output
stdOutputParser = pure StdOutput

-- | Parser for @FileInput@.
fileInputParser :: Parser Input
fileInputParser =
  FileInput
    <$> optional
          (strOption $ long "dir" <> short 'd' <> metavar "DIRECTORY" <> help
            "optional directory"
          )
    <*> strOption
          (long "file" <> short 'f' <> metavar "INPUT" <> help "Input file")

-- | Parser for @FileInput@.
fileOutputParser :: Parser Output
fileOutputParser =
  FileOutput
    <$> optional
          (strOption $ long "outdir" <> metavar "DIRECTORY" <> help
            "optional output directory"
          )
    <*> strOption
          (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file")

-- | Parser for @Input@.
inputParser :: Parser Input
inputParser = fileInputParser <|> s3InputParser <|> stdInputParser

-- | Parser for @S3Input@.
s3InputParser :: Parser Input
s3InputParser =
  S3Input
    <$> strOption
          (  long "region"
          <> short 'r'
          <> metavar "REGION"
          <> value "us-east-1"
          <> help "AWS Region"
          )
    <*> strOption
          (long "bucket" <> short 'b' <> metavar "BUCKET" <> help "S3 bucket")
    <*> strOption
          (long "key" <> short 'k' <> metavar "KEY" <> help "S3 location")

-- | Parser for @S3Output@.
s3OutputParser :: Parser Output
s3OutputParser =
  S3Output
    <$> strOption
          (long "outregion" <> metavar "OUTREGION" <> value "us-east-1" <> help
            "output AWS Region"
          )
    <*> strOption
          (long "outbucket" <> metavar "OUTBUCKET" <> help "output S3 bucket")
    <*> strOption
          (long "outkey" <> metavar "OUTPUTKEY" <> help "output S3 location")

-- | Parser for @Output@
outputParser :: Parser Output
outputParser = fileOutputParser <|> s3OutputParser <|> stdOutputParser

-- | Parser for @InputDecompression@
inputDecompressionParser :: Parser InputDecompression
inputDecompressionParser =
  flag' Decompress
        (long "decompress" <> short 'd' <> help "decompress gzipped input")
    <|> pure NoDecompress

-- | Parser for @OutputDecompression@
outputCompressionParser :: Parser OutputCompression
outputCompressionParser =
  flag' Compress (long "gzip" <> short 'z' <> help "compress output using gzip")
    <|> pure NoCompress

{-
  IO Location utilities
-}

{- |
A type containing the @'Location'@s for input and output.
See @'parseIOSpec'@ for creating a term of this type.
-}
data IOSpec = MkIOSpec {
     -- | @'Location'@ for input
     inputLocation  :: Location
     -- | @'Location'@ for output
   , outputLocation :: Location
   } deriving (Show)

{- |
Creates an @'IOSpec'@ from an @'Input'@ and @'Output'@.
If a @PartitionIndex@ is provided, then the  @'Input'@ and @'Output'@
are parsed via @'updateLocationWithPartitionIndex'@.

Examples:

>>> parseIOSpec Nothing StdInput StdOutput
Right (MkIOSpec {inputLocation = Std, outputLocation = Std})
>>> parseIOSpec (Just (MkPartitionIndex 1)) StdInput StdOutput
Right (MkIOSpec {inputLocation = Std, outputLocation = Std})
>>> parseIOSpec (Just (MkPartitionIndex 1)) (FileInput Nothing "hasklepias-examples/exampleData/exampleData%%n%%.jsonl") StdOutput
Right (MkIOSpec {inputLocation = Local "hasklepias-examples/exampleData/exampleData1.jsonl", outputLocation = Std})
>>> parseIOSpec (Just (MkPartitionIndex 1)) (FileInput Nothing "hasklepias-examples/exampleData/exampleData%%n%%.jsonl") (FileOutput Nothing "hasklepias-examples/exampleData/exampleCohort%%n%%.json")
Right (MkIOSpec {inputLocation = Local "hasklepias-examples/exampleData/exampleData1.jsonl", outputLocation = Local "hasklepias-examples/exampleData/exampleCohort1.json"})
>>> parseIOSpec (Just (MkPartitionIndex 1)) (FileInput Nothing "hasklepias-examples/exampleData/exampleData%%n%%.jsonl") (FileOutput Nothing "hasklepias-examples/exampleData/exampleCohort.json")
Left FailedToSplitByDelimiter
>>> parseIOSpec (Just (MkPartitionIndex (-1))) (FileInput Nothing "hasklepias-examples/exampleData/exampleData%%n%%.jsonl") StdOutput
Left NOutOfBounds
>>> parseIOSpec (Just (MkPartitionIndex 10)) (FileInput Nothing "hasklepias-examples/exampleData/exampleData%%1%%.jsonl") StdOutput
Left NOutOfBounds

-}
parseIOSpec :: Maybe PartitionIndex
  -> Input
  -> Output
  -> Either PartitionTemplateError IOSpec
parseIOSpec Nothing i o = Right $ MkIOSpec (inputToLocation i) (outputToLocation o)
parseIOSpec (Just pid) i o =
  uncurry MkIOSpec <$> involve (update $ inputToLocation i, update $ outputToLocation o)
  where update = flip updateLocationWithPartitionIndex pid
        -- TODO: I failed to think of an obvious (f a, f a) -> f (a, a) function
        involve (x, y) = case x of
          Left e -> Left e
          Right z1 -> case y of
            Left e2  -> Left e2
            Right z2 -> Right (z1, z2)

{-
  BLAMMO logging utilities

  The logSettingsParser, parseLogSettings, and endo functions below
  are basically copied from
  https://hackage.haskell.org/package/Blammo-1.0.2.3/docs/src/Blammo.Logging.LogSettings.Env.html#parse.
  I (BS 2022-09-28) couldn't find an easy to change the default settings/parser
  without parsing the LOG_DESTINATION variable again,
  hence I just copied the functions over.
  I opened related issue here:
  https://github.com/freckle/blammo/issues/20
-}

-- | information about using logging
logSettingsHelpDoc :: Doc
logSettingsHelpDoc =
  dullblue (bold "== Log Settings ==") <> linebreak <> [i|
  Users can control the logging behavior by setting environmental variables.
  The default for asclepias apps is to send logs to stderr in terminal format.
  To change the format to JSON (for example) set the format variable
  before calling the application, as in:

  ```
  export LOG_FORMAT=json
  ```

  For more information see,
  https://hackage.haskell.org/package/Blammo/docs/Blammo-Logging-LogSettings-Env.html

  Available environment variables:

  LOG_LEVEL                known log level (case insensitive)
  LOG_DESTINATION          stdout|stderr|@<path>
  LOG_FORMAT               tty|json
  LOG_COLOR                auto|always|never
  |]

-- | Sets the destination for the default Log settings to @stderr@,
-- rather than @stdout@.
ourDefaultLogSettings :: LogSettings
ourDefaultLogSettings =
  setLogSettingsDestination LogDestinationStderr defaultLogSettings

-- | Environmental variable parser
logSettingsParser :: Env.Parser Env.Error LogSettings
logSettingsParser = ($ ourDefaultLogSettings) . appEndo . mconcat <$> sequenceA
  [ Env.var (endo readLogLevels setLogSettingsLevels) "LOG_LEVEL" (Env.def mempty)
  , Env.var (endo readLogDestination setLogSettingsDestination) "LOG_DESTINATION" (Env.def mempty)
  , Env.var (endo readLogFormat setLogSettingsFormat) "LOG_FORMAT" (Env.def mempty)
  , Env.var (endo readLogColor setLogSettingsColor) "LOG_COLOR" (Env.def mempty)
  ]

-- | Parse @'Blammo.Logging.LogSettings'@.
parseLogSettings :: IO LogSettings
parseLogSettings = Env.parse id logSettingsParser

endo
  :: Env.AsUnread e
  => (String -> Either String a)
  -- ^ How to parse the value
  -> (a -> b -> b)
  -- ^ How to turn the parsed value into a setter
  -> Env.Reader e (Endo b)
endo reader setter x = first Env.unread $ Endo . setter <$> reader x

{-
  Utililties for processing (integer) indexed files, e.g.:
     file1, file2, file3, ...
-}

-- | Enumerates kinds of errors handled by @'parsePartitionFilePath'@.
data PartitionTemplateError =
    NOutOfBounds
  | NonPostiveWidth
  | FailedToParseWidth
  | FailedToSplitByDelimiter
  deriving (Eq, Show)

-- | The possible pattern for a partition template
data PartitionTemplatePattern =
    -- ^ Partition has fixed width
    FixedWidth Int
    -- ^ Partition does not have fixed width
  | NotFixedWidth deriving (Eq, Show)

-- internal function for @parsePartitionFilePath@
parsePartitionTemplate ::
     String
  -> FilePath
  -> Either PartitionTemplateError (String, PartitionTemplatePattern, String)
parsePartitionTemplate delim =
    checkParse
     <=< fmap (call3 headMay (\x -> (x `atMay` 1) >>= readTemplatePattern) (`atMay` 2))
      . (\x -> if length x == 3 then Right x else Left FailedToSplitByDelimiter)
      . splitOn delim
  where call3 f g h = (\((x, y), z) -> (x, y, z)) . ((f &&& g) &&& h)

        readTemplatePattern "n" = Just NotFixedWidth
        readTemplatePattern x   = fmap FixedWidth (readMaybe x)

        checkParse (Just x, y, Just z) = case y of
          Nothing -> Left FailedToParseWidth
          Just pat -> case pat of
            NotFixedWidth -> Right (x, pat, z)
            FixedWidth i  -> if i <= 0 then Left NonPostiveWidth else Right (x, pat, z)
        checkParse (_, _, _) = Left FailedToSplitByDelimiter

{- |
A utility for templating @'FilePath'@ with an integer value
This is useful for processing batches of numbered files.
The template pattern is @%%n%%@,
where @n@ is either an integer value or the character @n@.
If @n@ is a positive integer,
then the output is left-padded with zeros to a fixed width of @n@.
If the template variable simply @n@,
then the output is not left-padded.

The examples below show usage and possible errors that may occur.

>>> parsePartitionFilePath "file-%%n%%.json" 3
Right "file-3.json"
>>> parsePartitionFilePath "file-%%5%%.json" 3
Right "file-00003.json"
>>> parsePartitionFilePath "file-%%10%%.json" 3
Right "file-0000000003.json"
>>> parsePartitionFilePath "file-%%n%%.json" (-1)
Left NOutOfBounds
>>> parsePartitionFilePath "file-%%2%%.json" 99
Right "file-99.json"
>>> parsePartitionFilePath "file-%%2%%.json" 100
Left NOutOfBounds
>>> parsePartitionFilePath "file-%%ab%%.json" 3
Left FailedToParseWidth
>>> parsePartitionFilePath "file-%%-1%%.json" 3
Left NonPostiveWidth
>>> parsePartitionFilePath "file-%%1%%.json" 10
Left NOutOfBounds
>>> parsePartitionFilePath "file-%%1%%-%%.json" 10
Left FailedToSplitByDelimiter

-}
parsePartitionFilePath ::
     FilePath
  -> Int
  -> Either PartitionTemplateError FilePath
parsePartitionFilePath x n =
   parsePartitionTemplate "%%" x >>= fillTemplate n
   where
    fillTemplate i (x, FixedWidth n, z) =
      if 0 <= i && i <= 10 ^ n - 1
        then Right $ x <> formatToString (left n '0') i <> z
        else Left NOutOfBounds
    fillTemplate i (x, NotFixedWidth, z) =
      if  i < 0
        then Left NOutOfBounds
        else Right $ x <> show i <> z


-- | Newtype wrapper for creating Parser for the @partition-index@ cli option.
newtype PartitionIndex = MkPartitionIndex Int deriving (Eq, Show)

-- | Parser for @PartitionIndex@.
partitionIndexParser :: Parser PartitionIndex
partitionIndexParser =
  option
  (eitherReader
    (\s -> case readMaybe s :: Maybe Int of
      Nothing -> Left "partition-index value must be an integer"
      Just i ->
        if i < 0
          then Left "partition-index value must be non-negative"
          else Right (MkPartitionIndex i)
    ))
    (  long "partition-index"
    <> metavar "INT"
    <> help
    ("Non-negative integer value to pass an index to templated input/output paths. " <>
     "See Processing Indexed Files help in help text for more information.")
    )

-- | information on how to use the @PartitionIndex@ option
partitionIndexDoc :: Doc
partitionIndexDoc =
  dullblue (bold "=== Processing Indexed Files ===")
  <> linebreak
  <> [i|
  The following options have a basic templating functionality for processing
  indexed filenames (such as file1.json, file2.json, file3.json, etc):

  * --file
  * --output
  * --key
  * --outkey

  The template pattern is @%%n%%@, where @n@ is either an integer value
  or the character 'n'. If n is a positive integer, then the output is
  left-padded with zeros to a fixed width of n. If the template variable is
  simply 'n', then the output is not left-padded.

  A filepath may contain at most one template.

  When templated paths are used, the --partition-index option is interpolated
  into the path(s). If both input and output go to one of the options listed,
  then both paths must be templated.

  **EXAMPLES**

   Options
    --file=path/to/in-%%5%%.ext
    --output=path/to/out-%%5%%.ext
    --partition-index=1
   -----------------------
   Interpretation
    input file = path/to/in-00001.ext
    output file = path/to/out-00001.ext

   Options
    --file=path/to/in%%n%%.ext
    --output=path/to/out%%n%%.ext
    --partition-index=1
   -----------------------
   Interpretation
    input file = path/to/in1.ext
    output file = path/to/out1.ext

   Options
    --file=path/to/in-%%5%%.ext
    --output=path/to/out-%%5%%.ext
    --partition-index=1
   -----------------------
   Interpretation
    input file = path/to/in-00001.ext
    output file = path/to/out-00001.ext

   Options
   --file=path/to/in%%n%%.ext
   --outkey=path/to/out%%n%%.ext
   --partition-index=1
   -----------------------
   Interpretation
    input file = path/to/in1.ext
    output S3 key = path/to/out1.ext

  |]

{- |
Modifies a @'Location'@ given a @'PartitionIndex'@ in the following ways:

* @Std@: ignores the @PartitionIndex@ and returns @Std@
* @Local@: modifies the @FilePath@ with @'parsePartitionFilePath'@
* @S3@: modifies the @'Amazonka.S3.ObjectKey'@ with @'parsePartitionFilePath'@

Examples:

>>> updateLocationWithPartitionIndex Std (MkPartitionIndex 1)
Right Std
>>> updateLocationWithPartitionIndex (Local "foo-%%2%%") (MkPartitionIndex 1)
Right (Local "foo-01")
>>> updateLocationWithPartitionIndex (Local "foo-%%%") (MkPartitionIndex 1)
Left FailedToSplitByDelimiter
>>> updateLocationWithPartitionIndex (S3 "us-east-1" "myBucket" "foo-%%5%%") (MkPartitionIndex 1)
Right (S3 (Region' {fromRegion = "us-east-1"}) (BucketName "myBucket") (ObjectKey "foo-00001"))
>>> updateLocationWithPartitionIndex ( Local "hasklepias-examples/exampleData/exampleData%%n%%.jsonl") (MkPartitionIndex 1)
Right (Local "hasklepias-examples/exampleData/exampleData1.jsonl")

-}
updateLocationWithPartitionIndex ::
     Location
  -> PartitionIndex
  -> Either PartitionTemplateError Location
updateLocationWithPartitionIndex Std _ = Right Std
updateLocationWithPartitionIndex (Local f) (MkPartitionIndex i) =
  case parsePartitionFilePath f i of
    Left e  -> Left e
    Right x -> Right $ Local x
updateLocationWithPartitionIndex (S3 r b (ObjectKey o)) (MkPartitionIndex i) =
  case parsePartitionFilePath (T.unpack o) i of
    Left e  -> Left e
    Right x -> Right $ S3 r b (ObjectKey (T.pack x))
