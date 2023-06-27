{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Hasklepias.AppBuilder.CohortApp
-- Description : Functions for running a cohort application.
-- Copyright   : (c) Target RWE 2023
-- License     : BSD3
-- Maintainer  : bbrown@targetrwe.com
--               ljackman@targetrwe.com
--               dpritchard@targetrwe.com
module Hasklepias.CohortApp (cohortMain) where

import Aws (Configuration (..))
import qualified Aws
import qualified Aws.S3 as S3
import Blammo.Logging.Simple
import Codec.Compression.GZip
  ( CompressionLevel,
    compress,
    decompress,
  )
import Cohort.Cohort
  ( Cohort,
    CohortSpec,
    SubjId (..),
    Subject (..),
    eventsToSubject,
  )
import qualified Cohort.Core as CCore
import qualified Cohort.Output as COutput
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Aeson (ToJSON (..), Value (..), encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Binary (sinkLbs)
import Data.Foldable (foldrM)
import Data.Generics.Product (HasField (field))
import qualified Data.HashMap.Strict as HM
import qualified Data.Ini as Ini
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map, keys)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EventDataTheory
  ( Event,
    EventLineAble,
    Eventable,
    FromJSONEvent,
    LineParseError (..),
    ToJSONEvent,
    defaultParseEventLineOption,
    parseEventLinesL',
  )
import GHC.Arr (accumArray, assocs)
import Hasklepias.CohortApp.CohortAppCLI
import Lens.Micro (set, (<&>))
import Lens.Micro.Extras (view)
import Network.HTTP.Conduit
  ( RequestBody (..),
    newManager,
    responseBody,
    tlsManagerSettings,
  )
import Options.Applicative
import System.Directory
  ( doesFileExist,
    getHomeDirectory,
  )
import System.Exit
import System.FilePath ((</>))

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
data CohortSettings t m a = MkCohortSettings
  { cohortSpecs :: Map Text (CohortSpec t m a),
    cliOpts :: CohortCLIOpts
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
        Compress -> compress
        NoCompress -> id
  let writefun = case snd $ output opts of
        StdOutput -> liftIO . BL.putStr
        FileOutput x -> liftIO . BL.writeFile x
        S3Output cred b k -> \d' -> do
          hash <- putS3Object cred b k d'
          logInfo $ "Put request complete" :# ["etag" .= show hash]

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
        Decompress -> decompress
        NoDecompress -> id

  bs <-
    -- TODO: toStrict can be expensive: Only 'decompress' with default options
    -- prevents us from simply replacing BL with BS to use strict bytestrings.
    -- See #402
    BL.toStrict . decompFun <$> case snd $ input opts of
      StdInput -> liftIO BL.getContents
      FileInput x -> liftIO $ BL.readFile x
      S3Input cred b k -> do
        (d, meta) <- getS3Object cred b k
        -- TODO destr
        logInfo $
          "Get request complete"
            :# [ "object-last-modified" .= show (S3.omLastModified meta),
                 "etag" .= show (S3.omETag meta)
               ]
        pure d

  -- Invoke
  parseSubjects bs

{- UTILITIES -}

-- Input data utilities

-- TODO need to consider whether to use lazy or strict bytestrings here.

-- | Internal. Parse a 'ByteString' into event lines. Process successfully parsed lines
-- into subjects. Logs out lines that failed to parse.
-- Note input must be UTF-8. See documentation for @BS.'Bytestring'@.
parseSubjects :: (CohortConstraints t m a b) => BS.ByteString -> CohortApp t m a [Subject t m a]
parseSubjects bs = do
  ivopt <- asks (intervalOpt . cliOpts)
  let (errs, bss) = parseEventLinesL' ivopt bs
  let ss = eventsToSubject bss
  -- Note this does not exit.
  mapM_
    ( \(MkLineParseError (n, e)) ->
        logError $ "parse-error" :# ["line-number" .= n, "error" .= e]
    )
    errs
  pure ss

-- AWS utilities
-- TODO convert all fals to throwIO when we do an error handling full rework.

-- | Utility for 'getAwsConfig'. `Aws.loadCredentialsFromFile` does not read
-- the Ini file format. This does so in order to maintain compatibility with
-- standard aws tools, e.g. boto and the cli.
getAwsCredentialsIniFile :: CredentialsCfg -> CohortApp t m a (Maybe Aws.Credentials)
getAwsCredentialsIniFile (MkCredentialsCfg prof) = do
  -- TODO this will throw an exception if unable to get the Home dir.
  credfile <- liftIO $ (</> ".aws/credentials") <$> getHomeDirectory
  chk <- liftIO $ doesFileExist credfile
  -- NOTE: getAwsConfig will handle missing file error after checking other
  -- sources, if needed.
  if chk
    then do
      ini <- liftIO $ Ini.readIniFile credfile
      case ini of
        Left _ -> do
          logDebug $ "cred-file-parse" :# ["cred-file" .= credfile]
          -- IMPORTANT: Do not print or log the readIniFile error, as it could
          -- leak secrets.
          fail "Credentials file must be in format required by aws cli."
        Right d -> do
          let prof_cred = HM.lookup prof $ Ini.unIni d
          let creds = (,) <$> (HM.lookup "aws_access_key_id" =<< prof_cred) <*> (HM.lookup "aws_secret_access_key" =<< prof_cred)
          case creds of
            Nothing -> do
              logDebug $ "cred-file-missing-var" :# ["profile" .= prof]
              pure Nothing
            Just (i, k) -> Just <$> Aws.makeCredentials (TE.encodeUtf8 i) (TE.encodeUtf8 k)
    else do
      logError $ "cred-file-does-not-exist" :# ["cred-file" .= credfile]
      pure Nothing

-- | This is 'Aws.baseConfiguration' but with a different default credentials
-- location. This checks credentials from the following sources, from first to last:
-- environment variables, credentials for the "default" profile in the
-- [@aws@ cli supported format](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html)
-- within @$HOME/.aws@, and finally Ec2 instance metadata.
--
-- Some explanation about why we are replicating the 'Aws.baseConfiguration' logic here:
--
-- 'Aws.baseConfiguration' uses 'Aws.loadCredentialsFromFile' to fetch
-- credentials from that source should loading from environment variables fail.
-- However, that function uses a custom format, not the Ini file format the @aws@
-- cli uses, as described
-- [here](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html).
--
-- So, we can either require the user to provide credentials in the supported format,
-- or this module can provide a function to parse Ini files. We opted for the latter,
-- implemented in 'getAwsCredentialsIniFile' and using it here.
getAwsConfig :: CredentialsCfg -> CohortApp t m a Aws.Configuration
getAwsConfig cfg = do
  credfromenv <- Aws.loadCredentialsFromEnv
  creds <- case credfromenv of
    Nothing -> do
      logDebug $ "aws-credentials-env" :# ["cred-env" .= Null]
      credfromfile <- getAwsCredentialsIniFile cfg
      case credfromfile of
        Nothing -> do
          -- Some logging done in getAwsCredentialsIniFile
          logDebug $ "aws-credentials-file" :# ["cred-cfg" .= toJSON cfg]
          credim <- Aws.loadCredentialsFromInstanceMetadata
          case credim of
            Nothing -> do
              logError $ "credentials-not-found" :# ["cred-cfg" .= toJSON cfg]
              fail "Could not load Aws credentials from any source."
        Just cr -> pure cr
    Just cr -> pure cr
  pure $
    Aws.Configuration
      { timeInfo = Aws.Timestamp,
        credentials = creds,
        logger = Aws.defaultLog Aws.Warning,
        proxy = Nothing
      }

-- | Get an object from S3 as a @BL.'ByteString'@, and return it along with its metadata.
getS3Object :: CredentialsCfg -> BucketName -> ObjectKey -> CohortApp t m a (BL.ByteString, S3.ObjectMetadata)
getS3Object credcfg b k = do
  -- NOTE: This is largely copy-pasted from the Aws package top-level docs.
  -- Aws.baseConfiguration doesn't have the credentials defaults we want.
  cfg <- getAwsConfig credcfg
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
  -- TODO according to docs, creating a manager is "expensive." evaluate
  -- whether to use one (for retries) at all, and if so pass it as an
  -- argument to getS3Object, so it can be shared with putS3Object. likely
  -- "expensive" is nothing compared to processing a cohort.
  mgr <- liftIO $ newManager tlsManagerSettings
  liftIO $ runResourceT $ do
    rsp <- Aws.pureAws cfg s3cfg mgr $ S3.getObject b k
    d <- runConduit $ responseBody (S3.gorResponse rsp) .| sinkLbs
    pure (d, S3.gorMetadata rsp)

-- | Put an object to S3. Returns the object hash 'ETag'. See S3 [object
-- documentation](https://docs.aws.amazon.com/AmazonS3/latest/API/API_Object.html).
putS3Object :: CredentialsCfg -> BucketName -> ObjectKey -> BL.ByteString -> CohortApp t m a Text
putS3Object credcfg b k o = do
  cfg <- getAwsConfig credcfg
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
  mgr <- liftIO $ newManager tlsManagerSettings
  liftIO $ runResourceT $ do
    S3.PutObjectResponse {S3.porETag = etag} <- Aws.pureAws cfg s3cfg mgr $ S3.putObject b k (RequestBodyLBS o)
    pure etag
