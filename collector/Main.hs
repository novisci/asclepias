{-| An application for collecting cohorts run on different partitions of data
-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy.Char8    as C
                                                ( lines
                                                , putStrLn
                                                , toStrict
                                                )
import           Hasklepias                     ( runCollectionApp, getLocations, Input(..) )
import           Options.Applicative


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
  { input :: Input
  , ouput :: FilePath
  }

collector :: Parser CollectorApp
collector = CollectorApp <$> (fileInput <|> s3input) <*> strOption
  (long "output" <> short 'o' <> metavar "FILE" <> value "output.json" <> help
    "Output location"
  )

desc = 
  "Collects cohorts run on different input data. The cohorts must be derived \
  \from the same cohort specification or results my be weird. Supports reading \
  \data from a local directory or from S3. In either case the input is a path \
  \to a file containing paths (or S3 keys) to each cohort part, where One line \
  \= one file.\
  \\n\n\
  \S3 capabilities are currently limited (e.g. AWS region is set \
  \to N. Virginia)."


opts :: ParserInfo CollectorApp
opts = Options.Applicative.info
  (collector <**> helper)
  (fullDesc <> progDesc desc <> header "cohort collector")

main :: IO ()
main = do
  options <- execParser opts

  let files = input options
  fs <- getLocations files

  r  <- runCollectionApp fs
  C.putStrLn r
