{-|
Module      : Misc types and functions
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Hasklepias.AppUtilities
  ( Location(..)
  , Input(..)
  , Output(..)
  , readData
  , readDataStrict
  , writeData
  , writeDataStrict
  , getS3Object
  , inputToLocation
  , outputToLocation

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
  ) where

import           Amazonka                   (LogLevel (Debug, Error), Region,
                                             ToBody (..), discover, newEnv,
                                             newLogger, runResourceT, send,
                                             sinkBody)
import           Amazonka.S3                (BucketName,
                                             ObjectCannedACL (ObjectCannedACL_Bucket_owner_full_control),
                                             ObjectKey, newGetObject,
                                             newPutObject)
import           Blammo.Logging.LogSettings
import           Codec.Compression.GZip     (CompressionLevel, compress,
                                             decompress)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor             (first)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as BL hiding (putStrLn)
import           Data.Conduit.Binary        (sinkLbs)
import           Data.Generics.Product      (HasField (field))
import           Data.Semigroup             (Endo (..))
import           Data.String                (IsString (fromString))
import           Data.String.Interpolate    (i)
import qualified Data.Text                  as T (Text, pack)
import qualified Data.Text.IO               as T (putStrLn)
import qualified Env
import           Lens.Micro                 (set, (<&>), (^.))
import           Lens.Micro.Extras          (view)
import           Options.Applicative
import           Options.Applicative.Help
import           System.IO                  (stderr)

-- | Type representing locations that data can be read from
data Location where
  Std   ::Location
  Local ::FilePath -> Location
  S3    ::Region -> BucketName -> ObjectKey -> Location
  deriving (Show)

-- | Type to hold input information. Either from file or from S3.
data Input =
     StdInput
   | FileInput (Maybe FilePath) FilePath
   | S3Input  String BucketName ObjectKey
   deriving (Show)

-- | Type to hold input information. Either from file or from S3.
data Output =
     StdOutput
   | FileOutput (Maybe FilePath) FilePath
   | S3Output  String BucketName ObjectKey
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
inputToLocation (S3Input r b k) = S3 (fromString r) b k

-- | Maps an @Input@ to a @Location@.
outputToLocation :: Output -> Location
outputToLocation StdOutput        = Std
outputToLocation (FileOutput d f) = Local (pre f)
 where
  pre = case d of
    Nothing -> (<>) ""
    Just s  -> (<>) (s <> "/")
outputToLocation (S3Output r b k) = S3 (fromString r) b k

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

