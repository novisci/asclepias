{-|
Module      : Misc types and functions 
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Hasklepias.AppUtilities
  ( Location(..)
  , Input(..)
  , readData
  , readDataStrict
  , getS3Object
  , inputToLocation
  , stdInput
  , fileInput
  , s3Input
  ) where

import           Data.Eq                        ( Eq(..) )
import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.Either                    ( fromRight )
import           Data.Maybe                     ( Maybe(..), fromMaybe )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show(..) )

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Char8         as C
import           Data.Conduit.Binary            ( sinkLbs )
import Data.String
import Data.Text
import           GHC.IO
import           Lens.Micro                     ( (<&>)
                                                , (^.)
                                                , set
                                                )
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.S3
import           System.IO                      ( stderr )
import           Options.Applicative

-- | Type representing locations that data can be read from
data Location where
  StdIn ::Location
  Local ::FilePath -> Location
  S3    ::Region -> BucketName -> ObjectKey -> Location
  deriving (Show)

-- | Type to hold input information. Either from file or from S3. 
data Input =
     StdInput
   | FileInput (Maybe FilePath) FilePath
   | S3Input  String BucketName ObjectKey
   deriving (Show)

-- | Defines @IsString@ instance for @Region@. Sets the default to @NorthVirginia@ 
--   (us-east-1) if the region can't be parsed
instance IsString Region where
  fromString x = fromRight NorthVirginia (fromText (pack x))

-- | Read data from a @Location@ to lazy @ByteString@
readData :: Location -> IO B.ByteString
readData StdIn      = B.getContents
readData (Local x ) = B.readFile x
readData (S3 r b k) = getS3Object r b k

-- | Read data from a @Location@ to strict @ByteString@. 
readDataStrict :: Location -> IO C.ByteString
readDataStrict StdIn      = C.getContents
readDataStrict (Local x ) = C.readFile x
readDataStrict (S3 r b k) = fmap B.toStrict (getS3Object r b k)

-- | Get an object from S3. 
getS3Object :: Region -> BucketName -> ObjectKey -> IO B.ByteString
getS3Object r b k = do
  lgr <- newLogger Debug stderr
  env <- newEnv Discover <&> set envLogger lgr . set envRegion r
  runResourceT . runAWS env $ do
    result <- send $ getObject b k
    (result ^. gorsBody) `sinkBody` sinkLbs

-- | Maps an @Input@ to a @Location@.
inputToLocation :: Input -> Location
inputToLocation StdInput        = StdIn
inputToLocation (FileInput d f) = Local (pre f)
 where
  pre = case d of
    Nothing -> (<>) ""
    Just s  -> (<>) (s <> "/")
inputToLocation (S3Input r b k) = S3 (fromString r) b k

-- | Parser @StdInput@.
stdInput :: Parser Input
stdInput = pure StdInput

-- | Parser for @FileInput@.
fileInput :: Parser Input
fileInput =
  FileInput
    <$> optional
          (strOption $ long "dir" <> short 'd' <> metavar "DIRECTORY" <> help
            "optional directory"
          )
    <*> strOption
          (long "file" <> short 'f' <> metavar "INPUT" <> help "Input file")

-- | Parser for @S3Input@.
s3Input :: Parser Input
s3Input =
  S3Input
    <$> strOption
          (long "region" <> short 'r' <> metavar "REGION" <> value "us-east-1" <> help "AWS Region") 
    <*>   strOption
          (long "bucket" <> short 'b' <> metavar "Bucket" <> help "S3 bucket")
    <*> strOption
          (long "key" <> short 'k' <> metavar "KEY" <> help "S3 location")