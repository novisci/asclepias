-- |

module TestUtils.S3Utils
  ( s3Copy
  , s3RecursiveRm
  ) where

import           System.Process                 ( callCommand )

-- Copy a file, possibly from and/or to S3
s3Copy :: String -> String -> IO ()
s3Copy from to = callCommand cmd
  where cmd = "aws s3 cp " ++ from ++ " " ++ to

-- Delete all objects in S3 starting with the prefix `uri`
s3RecursiveRm :: String -> IO ()
s3RecursiveRm uri = callCommand cmd
  where cmd = "aws s3 rm --recursive " ++ uri
