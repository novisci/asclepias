{-|
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main
  ( main
  ) where

import Control.Lens
import System.IO
import           Network.AWS
import           Network.AWS.S3
import           Prelude
import           Data.Aeson
import           Hasklepias (CohortSetJSON)
import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.UTF8 as U
import qualified          Data.ByteString.Lazy.Char8    as C
                                                ( putStrLn, lines )
import Options.Applicative
import Data.Conduit
import qualified Data.Text as T (pack)
import Conduit
import           Data.Conduit.Binary (sinkLbs)
import qualified Data.Conduit.List as CL
import Data.Monoid (Sum (..))
import GHC.IO.Device (IODeviceType(Directory))

file1 :: FilePath
file1 = "collector/test1.json"

file2 :: FilePath
file2 = "collector/test2.json"

f :: FilePath -> IO (Either String CohortSetJSON)
f x = fmap eitherDecode (B.readFile x)

g :: FilePath -> FilePath -> IO (Either String CohortSetJSON)
g x y = liftA2 (liftA2 (<>)) (f x) (f y)

data Location where
  Local :: FilePath -> Location
  S3    :: BucketName -> ObjectKey -> Location

getData :: Location -> IO (Maybe CohortSetJSON)
getData (Local x) = decode <$> B.readFile x
getData (S3 b k)  = decode <$> doGetObject b k

-- reader :: BucketName -> FileName -> (FileName -> IO B.ByteString)
-- reader _ (L x) = \(L x) -> B.readFile x
-- reader b (S x) = \(S x) -> doGetObject b x

-- ff :: Path -> FileName -> IO (Maybe CohortSetJSON)
-- ff (L x) = fmap decode (reader "" (L x) (L x))
-- ff (S x) = fmap decode (reader "" (S x) (S x))

-- newtype Collector = MkCollector (Either String CohortSetJSON)

-- instance Semigroup  Collector where
--   (<>) (MkCollector x) (MkCollector y) = MkCollector (liftA2 (<>) x y)

-- data FileName =
--     L FilePath
--   | S ObjectKey

-- data Path =
--     FS FilePath
--   | S3 BucketName


doGetObject :: BucketName -> ObjectKey -> IO B.ByteString
doGetObject b k = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion NorthVirginia
    runResourceT . runAWS env $ do
      result <- send $ getObject b k
      (result ^. gorsBody) `sinkBody` sinkLbs


getLocations :: Input -> IO [Location]
getLocations (FileInput d f) =  fmap (fmap Local) (fmap pre . lines <$> readFile f)
    where pre = case d of
            Nothing -> (<>) ""
            Just s  -> (<>) (s <> "/")
getLocations (S3Input b k) = 
  fmap (\x -> S3 b (ObjectKey $ T.pack $ show x)) . C.lines <$> doGetObject b k


data Input =
     FileInput (Maybe FilePath) FilePath
   | S3Input BucketName ObjectKey
   deriving (Show)

fileInput :: Parser Input
fileInput = FileInput <$>
  optional (strOption
    $    long "dir"
      <> short 'd'
      <> metavar "DIRECTORY"
      <> help "optional directory"
  ) <*> strOption (
     long "file"
  <> short 'f'
  <> metavar "INPUT"
  <> help "Input file"
  )

s3input :: Parser Input
s3input = S3Input
  <$> strOption ( long "bucket" <> short 'b'   <> metavar "Bucket" <> help "S3 bucket" )
  <*> strOption ( long "manifest" <> short 'm' <> metavar "KEY" <> help "S3 manifest file" )
  -- <|> strOption ( long "prefix" <> short 'p'   <> metavar "Prefix" <> help "prefix" ) 

data CollectorApp = CollectorApp {
    input :: Input
  , ouput :: FilePath
  }

collector :: Parser CollectorApp
collector = CollectorApp  <$>
  ( fileInput <|> s3input) <*> strOption
  (    long "output"
    <> short 'o'
    <> metavar "FILE"
    <> value "output.json"
    <> help "Output location"
    )

opts :: ParserInfo CollectorApp
opts = Options.Applicative.info (collector <**> helper)
  ( fullDesc
  <> progDesc "TODO: add a description"
  <> header "cohort collector" )

main :: IO ()
main = do
  options <- execParser opts

  let files = input options
  fs <- getLocations files

  print files
  -- print fs

  putStrLn "\nconduit version"
  r <- runConduit
    $ yieldMany fs
    .| foldMapMC getData
  case r of
    Nothing -> putStrLn "something bad happened"
    Just x -> C.putStrLn (encode x)
