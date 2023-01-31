{-|
Module      : Hasklepias.AppBuilder.CohortApp
Description : Functions for running a cohort application.
Copyright   : (c) Target RWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com
              ljackman@targetrwe.com
              dpritchard@targetrwe.com
-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Hasklepias.CohortApp (cohortMain) where

import           Amazonka                          (ToBody (..), discover,
                                                    envRegion, newEnv,
                                                    runResourceT, send,
                                                    sinkBody)
import           Amazonka.S3                       (BucketName, ETag,
                                                    ObjectCannedACL (ObjectCannedACL_Bucket_owner_full_control),
                                                    ObjectKey,
                                                    PutObjectResponse, Region,
                                                    newGetObject, newPutObject)
import           Amazonka.S3.Lens                  (getObjectResponse_body,
                                                    getObjectResponse_eTag,
                                                    getObjectResponse_httpStatus,
                                                    putObjectResponse_eTag,
                                                    putObjectResponse_httpStatus,
                                                    putObject_acl)
import           Blammo.Logging.Simple
import           Codec.Compression.GZip            (CompressionLevel, compress,
                                                    decompress)
import           Cohort.Cohort                     (Cohort, CohortSpec,
                                                    SubjId (..), Subject (..),
                                                    eventsToSubject)
import qualified Cohort.Core                       as CCore
import qualified Cohort.Output                     as COutput
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Reader
import           Data.Aeson                        (ToJSON, encode)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BL
import           Data.Conduit.Binary               (sinkLbs)
import           Data.Foldable                     (foldrM)
import           Data.Generics.Product             (HasField (field))
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Map.Strict                   (Map, keys)
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           EventDataTheory                   (Event, EventLineAble,
                                                    Eventable, FromJSONEvent,
                                                    LineParseError (..),
                                                    ToJSONEvent,
                                                    defaultParseEventLineOption,
                                                    parseEventLinesL)
import           GHC.Arr                           (accumArray, assocs)
import           Hasklepias.CohortApp.CohortAppCLI
import           Lens.Micro                        (set, (<&>))
import           Lens.Micro.Extras                 (view)
import           Options.Applicative
import           System.Exit

-- | Create a command-line application for processing event-line data into
-- cohorts, using the logic provided in each 'CohortSpec'. See the top-level
-- `Hasklepias` module documentation for details.
cohortMain :: (CohortConstraints t m a b) => Map Text (CohortSpec t m a) -> IO ()
cohortMain specs = do
    opts <- execParser cliParserInfo
    let cfg = MkCohortSettings specs opts
    runCohortApp cohortApp cfg

{- INTERNAL: CohortApp -}

-- | Internal. Collecting the many constraints placed on `t m a` by
-- dependencies. Note the unused 'b' parameter is taken from 'IntervalSizeable
-- a b' but is unused here.  See interval-algebra issues.
type CohortConstraints t m a b = (Eventable t m a, EventLineAble t m a b, ToJSONEvent t m a, FromJSONEvent t m a)

-- | Internal. Configuration type for a `CohortApp`. `t m a` correspond to the
-- underlying @'Event' t m a@ parameters via the 'CohortSpec'. A project
-- developer can only configure the 'cohortSpecs' field. Remaining options are
-- accessible via the compiled command-line application only.
data CohortSettings t m a
  = MkCohortSettings
      { cohortSpecs :: Map Text (CohortSpec t m a)
      , cliOpts     :: CohortCLIOpts
      }

-- | Internal. Type used to run a cohort application pipeline. The user does
-- not have access to this type and can only run the entire pipeline, with
-- custom 'cohortSpecs', via 'cohortMain'.
type CohortApp t m a = ReaderT (CohortSettings t m a) (LoggingT IO)

-- | Internal.
runCohortApp :: CohortApp t m a b -> CohortSettings t m a -> IO b
runCohortApp app = runSimpleLoggingT . runReaderT app

{- INTERNAL: CohortApp pipeline
    * Wraps pure cohort-building pipeline from `Cohort`
    * Provides logging, top-level error handling
    * Data read/write actions
    -}

-- | Internal. A simple `CohortApp` pipeline, which consists of a read step, a
-- transformation step in which the pure cohort-building functions are applied
-- to data, and a write step. Each step should handle its own logging and
-- failure modes.
cohortApp :: (CohortConstraints t m a b) => CohortApp t m a ()
cohortApp = readData >>= evalCohortMap >>= writeData

-- | Internal. Evaluate the input data with respect to the user-provided
-- cohort-bilding logic in 'cohortSpecs'. If the input list is empty, fail
-- immediately with error code one.
evalCohortMap :: [Subject t m a] -> CohortApp t m a (Map Text (Cohort a))
evalCohortMap [] = do
    logError "No valid subject data to process. Check logs for line parse errors."
    liftIO $ exitWith (ExitFailure 1)
evalCohortMap subjs = do
    sm <- asks cohortSpecs
    logInfo $ "Building cohorts" :# ["cohorts" .= keys sm]
    pure $ CCore.evalCohortMap sm subjs

-- | Internal. Transform data to the output shape via @Cohort.'toCohortJSON'@
-- and write to specified location.
writeData :: (ToJSON a) => Map Text (Cohort a) -> CohortApp t m a ()
writeData d = do
    opts <- asks cliOpts
    logInfo $ "Writing data to" :# ["location" .= show (output opts)]
    let compfun = case outCompress opts of
                    Compress   -> compress
                    NoCompress -> id
    let writefun = case snd $ output opts of
                     StdOutput      -> liftIO . BL.putStr
                     FileOutput x -> liftIO . BL.writeFile x
                     S3Output r b k -> \d' -> do
                         (httpStatus, hash) <- putS3Object r b k d'
                         logInfo $ "Put request complete" :# ["put-http-status" .= show httpStatus
                                                             , "etag" .= maybe "" show hash
                                                             ]

    writefun . compfun . encode $ COutput.toCohortJSON d

-- | Internal. Read data from the specified location, attempt to parse as
-- @['EventLine']@, then convert to a list of subjects. Note the result
-- can be an empty list, and there is no check at this stage it is not. For
-- example, if no lines parse correctly the result will be @[]@.
readData :: (CohortConstraints t m a b) => CohortApp t m a [Subject t m a]
readData = do
    opts <- asks cliOpts
    logInfo $ "Reading data from" :# ["location" .= show (input opts)]

    let decompFun = case inDecompress opts of
                      Decompress   -> decompress
                      NoDecompress -> id

    bs <- decompFun <$> case snd $ input opts of
               StdInput      -> liftIO BL.getContents
               FileInput x -> liftIO $ BL.readFile x
               S3Input r b k -> do
                   (d, httpStatus, hash) <- getS3Object r b k
                   logInfo $ "Get request complete" :# ["get-http-status" .= show httpStatus
                                                       , "etag" .= maybe "" show hash
                                                       ]
                   pure d


    parseSubjects bs


    {- UTILITIES -}

-- Input data utilities

-- TODO need to consider whether to use lazy or strict bytestrings here.

-- | Internal. Parse a 'ByteString' into event lines. Process successfully parsed lines
-- into subjects. Logs out lines that failed to parse.
parseSubjects :: (CohortConstraints t m a b) => BL.ByteString -> CohortApp t m a [Subject t m a]
parseSubjects bs = do
    let (errs, bss) = parseEventLinesL defaultParseEventLineOption bs
    let ss = eventsToSubject bss
    -- Note this does not exit.
    mapM_
      (\(MkLineParseError (n,e)) ->
        logError $ "parse-error" :# ["line-number" .= n, "error" .= e]) errs
    pure ss


-- Amazonka utilities

-- | Get an object from S3 as a @BL.'ByteString'@. In addition, return the http
-- response code and (Maybe) the object hash 'ETag'. See S3 [object
-- documentation](https://docs.aws.amazon.com/AmazonS3/latest/API/API_Object.html).
getS3Object :: Region -> BucketName -> ObjectKey -> CohortApp t m a (BL.ByteString, Int, Maybe ETag)
getS3Object r b k = do
  env <- set (field @"envRegion") r <$> newEnv discover
  resp <- runResourceT $ send env (newGetObject b k)

  -- sinkLbs reads all content into memory strictly. ResponseBody type is a
  -- ConduitT stream.
  d <- view getObjectResponse_body resp `sinkBody` sinkLbs
  let httpStatus = view getObjectResponse_httpStatus resp
  let hash = view getObjectResponse_eTag resp
  pure (d, httpStatus, hash)


-- | Put an object to S3. Returns a tuple of the http respose code and (Maybe)
-- the object hash 'ETag'. See S3 [object
-- documentation](https://docs.aws.amazon.com/AmazonS3/latest/API/API_Object.html).
putS3Object :: Region -> BucketName -> ObjectKey -> BL.ByteString -> CohortApp t m a (Int, Maybe ETag)
putS3Object r b k o = do
  env <- set (field @"envRegion") r <$> newEnv discover
  -- AWS permissions docs:
  -- https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl
  let obj = set putObject_acl (Just ObjectCannedACL_Bucket_owner_full_control)
            (newPutObject b k (toBody o))
  resp <- runResourceT $ send env obj

  let hash = view putObjectResponse_eTag resp
  let httpStatus = view putObjectResponse_httpStatus resp
  pure (httpStatus, hash)
