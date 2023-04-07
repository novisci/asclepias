{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Hasklepias.AppBuilder.CohortAppCLI
-- Description : Internal module definining command-line options for a 'CohortApp'.
-- Copyright   : (c) Target RWE 2023
-- License     : BSD3
-- Maintainer  : bbrown@targetrwe.com
--               ljackman@targetrwe.com
--               dpritchard@targetrwe.com
module Hasklepias.CohortApp.CohortAppCLI where

import Data.Aeson (ToJSON)
import Data.String.Interpolate (i)
import Data.Text (Text, splitOn)
import GHC.Generics (Generic)
import Options.Applicative
import Options.Applicative.Help hiding (fullDesc)
import Text.Read (readMaybe)

-- | Internal. A type which contains the evaluation options of a cohort
-- application. These options are set at the command line.
data CohortCLIOpts = CohortCLIOpts
  { -- | Options for where to read @'Input'@.
    input :: !(InputFlag, Input),
    -- | Options for relevant output.
    output :: !(OutputFlag, Output),
    -- | Decompress gzipped input.
    inDecompress :: !InputDecompression,
    -- | Compress output using gzip.
    outCompress :: !OutputCompression
  }

{- TYPES -}

-- TODO it is annoying to have to specify --cred-file for both s3in and s3out.
-- make the option common if either one is used, or at least have one take
-- precedence that way the default for the s3out can be used.

-- AWS.S3.Core defines Bucket and Object as Text alias.
type BucketName = Text

type ObjectKey = Text

newtype CredentialsCfg = MkCredentialsCfg
  { credentialsProfile :: Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON CredentialsCfg

-- | Switch to declare input location.
data InputFlag = StdIn | FileIn | S3In deriving (Show)

-- | Type to hold input information. Either from file or from S3.
data Input
  = StdInput
  | FileInput FilePath
  | S3Input CredentialsCfg BucketName ObjectKey
  deriving (Show)

-- | Switch to declare output location.
data OutputFlag = StdOut | FileOut | S3Out deriving (Show)

-- | Type to hold input information. Either from file or from S3.
data Output
  = StdOutput
  | FileOutput FilePath
  | S3Output CredentialsCfg BucketName ObjectKey
  deriving (Show)

-- | Flag for whether to decompress input
data InputDecompression = NoDecompress | Decompress deriving (Show)

-- | Flag for whether to compress output
data OutputCompression = NoCompress | Compress deriving (Show)

{- PARSERS -}

-- | Internal.
cliParser :: Parser CohortCLIOpts
cliParser =
  CohortCLIOpts
    <$> inParser
    <*> outParser
    <*> inputDecompressionParser
    <*> outputCompressionParser

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
cliParserInfo =
  info
    (cliParser <**> helper)
    (fullDesc <> progDescDoc (Just helpText))
  where
    helpText = [i|A cohort-building application.|]

-- | StdIn flag.
stdInFlag :: Parser InputFlag
stdInFlag =
  flag'
    StdIn
    ( long "stdin"
        <> help "Read from stdin"
    )

-- | FileIn flag.
fileInFlag :: Parser InputFlag
fileInFlag =
  flag'
    FileIn
    ( long "filein"
        <> help "Read from specified INPUT file"
    )

-- | S3In flag.
s3InFlag :: Parser InputFlag
s3InFlag =
  flag'
    S3In
    ( long "s3in"
        <> help "Read from specified S3 object, using BUCKETIN KEYIN"
    )

-- | StdOut flag.
stdOutFlag :: Parser OutputFlag
stdOutFlag =
  flag'
    StdOut
    ( long "stdout"
        <> help "Write to stdout"
    )

-- | FileOut flag.
fileOutFlag :: Parser OutputFlag
fileOutFlag =
  flag'
    FileOut
    ( long "fileout"
        <> help "Write to specified OUTPUT file"
    )

-- | S3Out flag.
s3OutFlag :: Parser OutputFlag
s3OutFlag =
  flag'
    S3Out
    ( long "s3out"
        <> help "Write to specified S3 object, using BUCKETOUT KEYOUT"
    )

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
    <$> ( MkCredentialsCfg
            <$> strOption
              ( long "profile"
                  <> metavar "PROFILE"
                  <> value "default"
                  <> help "AWS profile name. Default is 'default'"
              )
        )
    <*> strOption
      (long "bkt-in" <> metavar "BUCKETIN" <> help "S3 input bucket")
    <*> strOption
      (long "key-in" <> metavar "KEYIN" <> help "S3 input object key")

-- | Parser for @S3Output@.
s3OutputParser :: Parser Output
s3OutputParser =
  S3Output
    <$> ( MkCredentialsCfg
            <$> strOption
              ( long "profile"
                  <> metavar "PROFILE"
                  <> value "default"
                  <> help "AWS profile name. Default is 'default'"
              )
        )
    <*> strOption
      (long "bkt-out" <> metavar "BUCKETOUT" <> help "S3 output bucket")
    <*> strOption
      (long "key-out" <> metavar "KEYOUT" <> help "S3 output object key")

-- | Parser for @InputDecompression@
inputDecompressionParser :: Parser InputDecompression
inputDecompressionParser =
  flag'
    Decompress
    (long "decompress" <> short 'd' <> help "Decompress gzipped input")
    <|> pure NoDecompress

-- | Parser for @OutputDecompression@
outputCompressionParser :: Parser OutputCompression
outputCompressionParser =
  flag' Compress (long "compress" <> short 'z' <> help "Ccompress output using gzip")
    <|> pure NoCompress
