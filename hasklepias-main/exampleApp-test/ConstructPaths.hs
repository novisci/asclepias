-- |

module ConstructPaths
  ( localResultsDir
  , localTestDataDir
  , s3Bucket
  , s3RootDir
  ) where

localTestDataDir :: String
localTestDataDir = "exampleApp-test/test/"

localResultsDir :: String
localResultsDir = "exampleApp-test/results/"

s3Bucket :: String
s3Bucket = "download.novisci.com"

s3RootDir :: String
s3RootDir = "hasklepias/sandbox-testapps/cohortApp/"
