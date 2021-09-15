{-|
-}

{-# LANGUAGE GADTs #-}

module CohortCollection 
  ( runCollectionApp
  , getLocations
  , Location(..)
  , Input(..)
  ) where

import           Conduit                        ( (.|)
                                                , foldMapMC
                                                , runConduit
                                                , runResourceT
                                                , yieldMany
                                                )
import           Control.Lens                   ( (<&>)
                                                , (^.)
                                                , set
                                                )
import           Data.Aeson                     ( decode
                                                , encode
                                                )
import qualified Data.ByteString.Char8         as CH
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C
                                                ( lines
                                                , putStrLn
                                                , toStrict
                                                )
import           Data.Conduit.Binary            ( sinkLbs )
import qualified Data.Conduit.List             as CL
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
                                                ( pack )
import           Cohort.Output                     ( CohortSetJSON )
import           Network.AWS
import           Network.AWS.S3
import           System.IO                      ( stderr )


data Location where
  Local ::FilePath -> Location
  S3    ::BucketName -> ObjectKey -> Location

getData :: Location -> IO (Maybe CohortSetJSON)
getData (Local x) = decode <$> B.readFile x
getData (S3 b k ) = decode <$> doGetObject b k

doGetObject :: BucketName -> ObjectKey -> IO B.ByteString
doGetObject b k = do
  lgr <- newLogger Debug stderr
  env <- newEnv Discover <&> set envLogger lgr . set envRegion NorthVirginia
  runResourceT . runAWS env $ do
    result <- send $ getObject b k
    (result ^. gorsBody) `sinkBody` sinkLbs

getLocations :: Input -> IO [Location]
getLocations (FileInput d f) = fmap (fmap Local)
                                    (fmap pre . lines <$> readFile f)
 where
  pre = case d of
    Nothing -> (<>) ""
    Just s  -> (<>) (s <> "/")
getLocations (S3Input b k) =
  fmap (\x -> S3 b (ObjectKey $ T.pack $ CH.unpack $ C.toStrict x))
    .   C.lines
    <$> doGetObject b k

-- | 
data Input =
     FileInput (Maybe FilePath) FilePath
   | S3Input BucketName ObjectKey
   deriving (Show)

-- | Run collection on a list of 'Location's
runCollectionApp :: [Location] -> IO B.ByteString
runCollectionApp fs = do
  r <- runConduit $ yieldMany fs .| foldMapMC getData
  let x = fmap encode r
  let z = fromMaybe B.empty x
  return z
