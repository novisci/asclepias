module TestUtils.SessionId
  ( getSessionId
  ) where

import           Control.Monad                 ( guard )
import           Control.Exception             ( tryJust )
import           Data.Time                     ( getCurrentTime )
import           Data.Time.Clock.POSIX         ( utcTimeToPOSIXSeconds )
import           Data.Time.Clock               ( nominalDiffTimeToSeconds )
import           System.Environment ( getEnv )
import           System.IO.Error ( isDoesNotExistError )

-- Create a unique ID based on the GitLab environmental variable $CI_PIPELINE_ID
-- if one is defined, otherwise the computation fails with `isDoesNotExistError`
getCIPipelineId :: IO String
getCIPipelineId = getEnv "CI_PIPELINE_ID"

-- Use the value of `getCIPipelineId` if it was able to be obtained, otherwise use
-- the number of seconds since the epoch as a fallback
getSessionId :: IO String
getSessionId = do
  r <- tryJust (guard . isDoesNotExistError) getCIPipelineId
  case r of
    Left  e -> fmap (show . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) getCurrentTime
    Right v -> getCIPipelineId
