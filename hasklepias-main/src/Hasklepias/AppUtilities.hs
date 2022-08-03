{-|
Module      : Misc types and functions 
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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

  -- ** Compression handling
  , InputDecompression(..)
  , OutputCompression(..)

  -- ** CLI option parsers
  , stdInput
  , fileInput
  , s3Input
  , stdOutput
  , fileOutput
  , s3Output
  , inputDecompressionParser
  , outputCompressionParser
  ) where

import           Codec.Compression.GZip         ( CompressionLevel
                                                , compress
                                                , decompress
                                                )
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Lazy          as BL
                                         hiding ( putStrLn )
import           Data.Conduit.Binary            ( sinkLbs )
import           Data.Either                    ( fromRight )
import           Data.Generics.Product          ( HasField(field) )
import           Data.String                    ( IsString(fromString) )
import qualified Data.Text                     as T
                                                ( Text
                                                , pack
                                                )
import qualified Data.Text.IO                  as T
                                                ( putStrLn )
import           Lens.Micro                     ( (<&>)
                                                , (^.)
                                                , set
                                                )
import           Lens.Micro.Extras              ( view )
import           Options.Applicative

import qualified Control.Monad.Trans.AWS       as AWS

-- imports for amazonka < 2
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.S3
import           System.IO                      ( stderr )


-- IMPORTS for amazonka 2.0
-- import           System.IO                      ( stderr )
-- import           Amazonka                       ( Credentials(Discover)
--                                                 , LogLevel(Debug, Error)
--                                                 , Region
--                                                 , ToBody(..)
--                                                 , newEnv
--                                                 , newLogger
--                                                 , runResourceT
--                                                 , send
--                                                 , sinkBody
--                                                 )
-- import           Amazonka.S3                    ( BucketName
--                                                 , ObjectCannedACL
--                                                   ( ObjectCannedACL_Bucket_owner_full_control
--                                                   )
--                                                 , ObjectKey
--                                                 , newGetObject
--                                                 , newPutObject
--                                                 )

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

-- for amazonka < 2
-- | Defines @IsString@ instance for @Region@. Sets the default to @NorthVirginia@ 
--   (us-east-1) if the region can't be parsed
instance IsString Region where
  fromString x = fromRight NorthVirginia (fromText (T.pack x))

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
  lgr <- newLogger Error stderr
  env <- newEnv Discover <&> set envLogger lgr . set envRegion r
  runResourceT . runAWS env $ do
    result <- send $ getObject b k
    (result ^. gorsBody) `sinkBody` sinkLbs

-- amazonka 2.0...
-- getS3Object :: Region -> BucketName -> ObjectKey -> IO BL.ByteString
-- getS3Object r b k = do
--   lgr <- newLogger Debug stderr
--   env <-
--     newEnv Discover
--     <&> set (field @"_envLogger") lgr
--     .   set (field @"_envRegion") r
--   runResourceT $ do
--     result <- send env (newGetObject b k)
--     view (field @"body") result `sinkBody` sinkLbs

-- | Put an object on S3. 
--
-- NOTE: the put request uses the bucket-owner-full-control 
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html canned ACL>.
-- 

class (ToBody a) => PutS3 a where
  putS3Object :: Region -> BucketName -> ObjectKey -> a -> IO ()
  putS3Object r b k o = do
    lgr <- newLogger Error stderr
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r
    AWS.runResourceT . AWS.runAWST env $ do
      void . send $ set poACL (Just OBucketOwnerFullControl) (putObject b k (toBody o))
      liftIO
        .  T.putStrLn
        $  "Successfully Uploaded contents to "
        <> T.pack (show b)
        <> " - "
        <> T.pack (show k)

-- amazonka 2.0...
-- class (ToBody a) => PutS3 a where
--   putS3Object :: Region -> BucketName -> ObjectKey -> a -> IO ()
--   putS3Object r b k o = do
--     lgr <- newLogger Error stderr
--     env <-
--       newEnv Discover
--       <&> set (field @"_envLogger") lgr
--       .   set (field @"_envRegion") r
--     let obj = set (field @"acl") (Just ObjectCannedACL_Bucket_owner_full_control)
--               (newPutObject b k (toBody o))
--     runResourceT $ do
--       void . send env $ obj
--       liftIO . T.putStrLn $
--           "Successfully Uploaded contents to "
--         <> T.pack (show b)
--         <> " - "
--         <> T.pack (show k)

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

-- | Parser for @InputDecompression@
inputDecompressionParser :: Parser InputDecompression
inputDecompressionParser =
  (Decompress <$ strOption
      (long "decompress" <> short 'd' <> help "decompress gzipped input")
    )
    <|> pure NoDecompress

-- | Parser for @OutputDecompression@
outputCompressionParser :: Parser OutputCompression
outputCompressionParser =
  (Compress <$ strOption
      (long "gzip" <> short 'z' <> help "compress output using gzip")
    )
    <|> pure NoCompress
