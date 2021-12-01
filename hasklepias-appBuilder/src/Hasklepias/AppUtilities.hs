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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

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

import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show(..) )

import           Amazonka
import           Amazonka.Core                  ( LogLevel(Debug)
                                                , Region
                                                , sinkBody
                                                )
import           Amazonka.S3                    ( BucketName
                                                , ObjectKey
                                                , newGetObject
                                                )
import           Control.Applicative            ( (<$>)
                                                , Applicative((<*>), pure)
                                                , optional
                                                )
import           Control.Monad
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as B
import           Data.Conduit.Binary            ( sinkLbs )
import           Data.Generics.Product
import           Data.String
import           GHC.IO
import           Lens.Micro                     ( (<&>)
                                                , (^.)
                                                , set
                                                )
import           Lens.Micro.Extras              ( view )
import           Options.Applicative
import           System.IO                      ( stderr )

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
  env <-
    newEnv Discover
    <&> set (field @"_envLogger") lgr
    .   set (field @"_envRegion") r
  runResourceT $ do
    result <- send env (newGetObject b k)
    view (field @"body") result `sinkBody` sinkLbs

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
          (  long "region"
          <> short 'r'
          <> metavar "REGION"
          <> value "us-east-1"
          <> help "AWS Region"
          )
    <*> strOption
          (long "bucket" <> short 'b' <> metavar "Bucket" <> help "S3 bucket")
    <*> strOption
          (long "key" <> short 'k' <> metavar "KEY" <> help "S3 location")
