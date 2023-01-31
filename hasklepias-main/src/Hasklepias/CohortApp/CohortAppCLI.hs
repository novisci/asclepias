{-|
Module      : Hasklepias.AppBuilder.CohortAppCLI
Description : Internal module definining command-line options for a 'CohortApp'.
Copyright   : (c) Target RWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com
              ljackman@targetrwe.com 
              dpritchard@targetrwe.com
-}

{-# LANGUAGE QuasiQuotes #-}
module Hasklepias.CohortApp.CohortAppCLI where

import           Amazonka.S3              (BucketName, ObjectKey, Region)
import           Data.String.Interpolate  (i)
import           Data.Text                (Text, splitOn)
import           Options.Applicative
import           Options.Applicative.Help hiding (fullDesc)
import           Text.Read                (readMaybe)

-- | Internal. A type which contains the evaluation options of a cohort
-- application. These options are set at the command line.
data CohortCLIOpts
  = CohortCLIOpts
      -- NOTE the io switches are separate to allow declaration of intent with
      -- a single explicit flag, e.g. --stdin, and not with some less-obvious
      -- combination of flags (e.g. --input being mutually exclsive with the s3
      -- input flags).
      -- the application (in `readData` for example) does not need to
      -- check *both* the flag and the data elements. the options parser will
      -- guarantee the flag determines which options are appropriate, e.g.,
      -- --s3in only accepts the s3InputParser options. No type safety is lost
      -- either, since there can be only one variant instantiated for Input or
      -- Output sum types, as usual.
      { -- | Options for where to read @'Input'@.
        input :: !(InputFlag, Input)
        -- | Options for relevant output.
      , output       :: !(OutputFlag, Output)
        -- | Decompress gzipped input.
      , inDecompress :: !InputDecompression
        -- | Compress output using gzip.
      , outCompress  :: !OutputCompression
      }

{- TYPES -}

-- | Switch to declare input location.
data InputFlag = StdIn | FileIn | S3In deriving (Show)

-- | Type to hold input information. Either from file or from S3.
data Input
  = StdInput
  | FileInput FilePath
  | S3Input Region BucketName ObjectKey
  deriving (Show)

-- | Switch to declare output location.
data OutputFlag = StdOut | FileOut | S3Out deriving (Show)

-- | Type to hold input information. Either from file or from S3.
data Output
  = StdOutput
  | FileOutput FilePath
  | S3Output Region BucketName ObjectKey
  deriving (Show)

-- | Flag for whether to decompress input
data InputDecompression = NoDecompress | Decompress deriving (Show)

-- | Flag for whether to compress output
data OutputCompression = NoCompress | Compress deriving (Show)

{- PARSERS -}

-- | Internal.
cliParser :: Parser CohortCLIOpts
cliParser = CohortCLIOpts <$> 
  inParser <*> 
    outParser <*> 
      inputDecompressionParser <*> 
        outputCompressionParser

-- | Internal. Input type option parsers.
stdIn = (,) <$> stdInFlag <*> stdInputParser
fileIn = (,) <$> fileInFlag <*> fileInputParser
s3In = (,) <$> s3InFlag <*> s3InputParser

-- | Internal. Output options parsers.
stdOut = (,) <$> stdOutFlag <*> stdOutputParser
fileOut = (,) <$> fileOutFlag <*> fileOutputParser
s3Out = (,) <$> s3OutFlag <*> s3OutputParser

-- | Internal. Input options parsers.
inParser :: Parser (InputFlag, Input)
inParser = stdIn <|> fileIn <|> s3In

outParser :: Parser (OutputFlag, Output)
outParser = stdOut <|> fileOut <|> s3Out

-- | Internal. Argument to 'execParser'.
cliParserInfo :: ParserInfo CohortCLIOpts
cliParserInfo = info
  (cliParser <**> helper)
  (fullDesc <> progDescDoc (Just helpText))
 where
   helpText = [i|A cohort-building application.|]

-- | StdIn flag.
stdInFlag :: Parser InputFlag
stdInFlag = flag' StdIn (long "stdin" <> 
  help "Read from stdin")

-- | FileIn flag.
fileInFlag :: Parser InputFlag
fileInFlag = flag' FileIn (long "filein" <> 
  help "Read from specified INPUT file")

-- | S3In flag.
s3InFlag :: Parser InputFlag
s3InFlag = flag' S3In (long "s3in" <>
  help "Read from specified S3 object, using BUCKETIN KEYIN")

-- | StdOut flag.
stdOutFlag :: Parser OutputFlag
stdOutFlag = flag' StdOut (long "stdout" <> 
  help "Write to stdout")

-- | FileOut flag.
fileOutFlag :: Parser OutputFlag
fileOutFlag = flag' FileOut (long "fileout" <> 
  help "Write to specified OUTPUT file")

-- | S3Out flag.
s3OutFlag :: Parser OutputFlag
s3OutFlag = flag' S3Out (long "s3out" <>
  help "Write to specified S3 object, using BUCKETOUT KEYOUT")

-- NOTE these should be placed last in the Input/Output parser alternatives.
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
    <$> strOption
          (long "input" <> short 'i' <> metavar "INPUT" <> help "Input file")

-- | Parser for @FileInput@.
fileOutputParser :: Parser Output
fileOutputParser =
  FileOutput
    <$> strOption
          (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file")

-- | Parser for @S3Input@.
s3InputParser :: Parser Input
s3InputParser =
  S3Input
    <$> strOption
          (  long "reg-in"
          <> metavar "REGIONIN"
          <> value "us-east-1"
          <> help "AWS input region"
          )
    <*> strOption
          (long "bkt-in" <> metavar "BUCKETIN" <> help "S3 input bucket")
    <*> strOption
          (long "key-in" <> metavar "KEYIN" <> help "S3 input object key")

-- | Parser for @S3Output@.
s3OutputParser :: Parser Output
s3OutputParser =
  S3Output
    <$> strOption
          (long "reg-out" <> metavar "REGIONOUT" <> value "us-east-1" <> help
            "AWS output region"
          )
    <*> strOption
          (long "bkt-out" <> metavar "BUCKETOUT" <> help "S3 output bucket")
    <*> strOption
          (long "key-out" <> metavar "KEYOUT" <> help "S3 output object key")

-- | Parser for @InputDecompression@
inputDecompressionParser :: Parser InputDecompression
inputDecompressionParser =
  flag' Decompress
        (long "decompress" <> short 'd' <> help "Decompress gzipped input")
    <|> pure NoDecompress

-- | Parser for @OutputDecompression@
outputCompressionParser :: Parser OutputCompression
outputCompressionParser =
  flag' Compress (long "compress" <> short 'z' <> help "Ccompress output using gzip")
    <|> pure NoCompress
