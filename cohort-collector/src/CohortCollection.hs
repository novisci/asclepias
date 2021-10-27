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
import qualified Data.Conduit.List             as CL
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
                                                ( pack )
import           Cohort.Output                  ( CohortSetJSON )
import           Network.AWS
import           Network.AWS.S3
import           System.IO                      ( stderr )
import           Hasklepias.AppUtilities hiding (Input(..))


getCohortData :: Location -> IO (Maybe CohortSetJSON)
getCohortData x = fmap decode (readData x)

getLocations :: Input -> IO [Location]
getLocations (FileInput d f) = fmap (fmap Local)
                                    (fmap pre . lines <$> readFile f)
 where
  pre = case d of
    Nothing -> (<>) ""
    Just s  -> (<>) (s <> "/")
getLocations (S3Input b k) =
  fmap (\x -> S3 NorthVirginia b (ObjectKey $ T.pack $ CH.unpack $ C.toStrict x))
    .   C.lines
    <$> getS3Object NorthVirginia b k

-- | Type to hold input information. Either from file or from S3. 
data Input =
     FileInput (Maybe FilePath) FilePath
   | S3Input BucketName ObjectKey
   deriving (Show)

-- | Run collection on a list of 'Location's
runCollectionApp :: [Location] -> IO B.ByteString
runCollectionApp fs = do
  r <- runConduit $ yieldMany fs .| foldMapMC getCohortData
  let x = fmap encode r
  let z = fromMaybe B.empty x
  return z
