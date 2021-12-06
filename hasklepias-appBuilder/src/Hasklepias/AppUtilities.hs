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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

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
  , stdInput
  , fileInput
  , s3Input
  , stdOutput
  , fileOutput
  , s3Output
  ) where

import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Text                      as T ( pack )
import qualified Data.Text.IO                   as T ( putStrLn ) 
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show(..) )

import           Amazonka                       ( Credentials(Discover)
                                                , LogLevel(Debug, Error)
                                                , Region
                                                , ToBody(..)
                                                , newEnv
                                                , newLogger
                                                , runResourceT
                                                , send
                                                , sinkBody
                                                )
import           Amazonka.S3                    ( BucketName
                                                , ObjectCannedACL
                                                  ( ObjectCannedACL_Bucket_owner_full_control
                                                  )
                                                , ObjectKey
                                                , newGetObject
                                                , newPutObject
                                                )

import           Control.Applicative            ( (<$>)
                                                , Applicative((<*>), pure)
                                                , optional
                                                )
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as B
                                         hiding ( putStrLn )
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Conduit.Binary            ( sinkLbs )
import           Data.Generics.Product          ( HasField(field) )
import           Data.String                    ( IsString(fromString)
                                                , String
                                                )
import           GHC.IO                         ( FilePath
                                                , IO
                                                )
import           Lens.Micro                     ( (<&>)
                                                , (^.)
                                                , set
                                                )
import           Lens.Micro.Extras              ( view )
import           Options.Applicative
import           System.IO                      (stderr
                                                )



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
   deriving (Show)

-- | Read data from a @Location@ to lazy @ByteString@
readData :: Location -> IO B.ByteString
readData Std        = B.getContents
readData (Local x ) = B.readFile x
readData (S3 r b k) = getS3Object r b k

-- | Write data from a @Location@ to lazy @ByteString@
writeData :: Location -> B.ByteString -> IO ()
writeData Std        x = B.putStrLn x
writeData (Local f ) x = B.writeFile f x
writeData (S3 r b k) x = putS3Object r b k x

-- | Read data from a @Location@ to strict @ByteString@. 
readDataStrict :: Location -> IO C.ByteString
readDataStrict Std        = C.getContents
readDataStrict (Local x ) = C.readFile x
readDataStrict (S3 r b k) = fmap B.toStrict (getS3Object r b k)

-- | Write data from a @Location@ to strict @ByteString@. 
writeDataStrict :: Location -> C.ByteString -> IO ()
writeDataStrict Std        x = C.putStrLn x
writeDataStrict (Local f ) x = C.writeFile f x
writeDataStrict (S3 r b k) x = putS3Object r b k x

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
      newEnv Discover
      <&> set (field @"_envLogger") lgr
      .   set (field @"_envRegion") r
    let obj = set (field @"acl") (Just ObjectCannedACL_Bucket_owner_full_control)
              (newPutObject b k (toBody o))
    runResourceT $ do
      void . send env $ obj
      liftIO . T.putStrLn $
          "Successfully Uploaded contents to "
        <> T.pack (show b)
        <> " - "
        <> T.pack (show k)

instance PutS3 B.ByteString
instance PutS3 C.ByteString

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

-- | Parser @StdInput@.
stdInput :: Parser Input
stdInput = pure StdInput

-- | Parser @StdOutput@.
stdOutput :: Parser Output
stdOutput = pure StdOutput

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

-- | Parser for @FileInput@.
fileOutput :: Parser Output
fileOutput =
  FileOutput
    <$> optional
          (strOption $ long "outdir" <> metavar "DIRECTORY" <> help
            "optional output directory"
          )
    <*> strOption
          (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file")

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
          (long "bucket" <> short 'b' <> metavar "BUCKET" <> help "S3 bucket")
    <*> strOption
          (long "key" <> short 'k' <> metavar "KEY" <> help "S3 location")

-- | Parser for @S3Output@.
s3Output :: Parser Output
s3Output =
  S3Output
    <$> strOption
          (long "outregion" <> metavar "OUTREGION" <> value "us-east-1" <> help
            "output AWS Region"
          )
    <*> strOption
          (long "outbucket" <> metavar "OUTBUCKET" <> help "output S3 bucket")
    <*> strOption
          (long "outkey" <> metavar "OUTPUTKEY" <> help "output S3 location")


