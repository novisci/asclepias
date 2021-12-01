{-|
Module      : Hasklepias.MakeApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}

module Hasklepias.MakeApp
  ( CohortApp(..)
  , makeCohortApp
  , shapeOutput
  , runApp
  , runAppWithLocation
  ) where

import           Control.Applicative            ( Applicative )
import           Control.Monad                  ( (=<<)
                                                , Functor(fmap)
                                                , Monad(..)
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                , decode
                                                , encode
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.Map.Strict                ( fromList
                                                , toList
                                                )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( Monoid(mconcat) )
import           Data.String                    ( String )
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           GHC.IO                         ( FilePath
                                                , IO
                                                )
import           GHC.Show                       ( Show(show) )

import           Cohort
import           EventData                      ( Events )
import           IntervalAlgebra                ( IntervalSizeable )

import           Colog                          ( (<&)
                                                , (>$)
                                                , HasLog(..)
                                                , LogAction(..)
                                                , Message
                                                , WithLog
                                                , log
                                                , logError
                                                , logInfo
                                                , logPrint
                                                , logPrintStderr
                                                , logStringStderr
                                                , logStringStdout
                                                , logText
                                                , richMessageAction
                                                , withLog
                                                )
import qualified Data.ByteString.Char8         as CH
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C
                                                ( lines
                                                , putStrLn
                                                , toStrict
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Hasklepias.AppUtilities
import           Options.Applicative



data MakeCohort = MakeCohort
  { input :: Input
  , ouput :: FilePath
  }

mainOptions :: Parser MakeCohort
mainOptions = MakeCohort <$> (fileInput <|> s3Input <|> stdInput) <*> strOption
  (long "output" <> short 'o' <> metavar "FILE" <> value "output.json" <> help
    "Output location"
  )

makeAppArgs :: String -> String -> ParserInfo MakeCohort
makeAppArgs name version = Options.Applicative.info
  (mainOptions <**> helper)
  (fullDesc <> header (name <> " " <> version))

-- | Creates a cohort builder function
makeCohortBuilder
  :: ( FromJSON a
     , Show a
     , IntervalSizeable a b
     , ToJSON d0
     , ShapeCohort d0
     , Monad m
     )
  => CohortSetSpec (Events a) d0 i a
  -> m (B.ByteString -> m ([SubjectParseError], CohortSet d0))
makeCohortBuilder specs =
  return (return . second (evalCohortSet specs) . parsePopulationLines)

reshapeCohortSet :: (Cohort d0 -> CohortJSON) -> CohortSet d0 -> CohortSetJSON
reshapeCohortSet g x =
  MkCohortSetJSON $ fromList $ fmap (fmap g) (toList $ getCohortSet x)

shapeOutput
  :: (Monad m, ShapeCohort d0)
  => (Cohort d0 -> CohortJSON)
  -> m ([SubjectParseError], CohortSet d0)
  -> m ([SubjectParseError], CohortSetJSON)
shapeOutput shape = fmap (fmap (reshapeCohortSet shape))

-- logging based on example here:
-- https://github.com/kowainik/co-log/blob/main/co-log/tutorials/Main.hs
parseErrorL :: LogAction IO SubjectParseError
parseErrorL = logPrintStderr

logParseErrors :: [SubjectParseError] -> IO ()
logParseErrors x = mconcat $ fmap (parseErrorL <&) x

-- | Type containing the cohort app
newtype CohortApp m = MkCohortApp { runCohortApp :: Maybe Location -> m B.ByteString }

-- | Make a command line cohort building application.
makeCohortApp
  :: (FromJSON a, Show a, IntervalSizeable a b, ToJSON d0, ShapeCohort d0)
  => String  -- ^ cohort name
  -> String  -- ^ app version
  -> (Cohort d0 -> CohortJSON) -- ^ a function which specifies the output shape
  -> CohortSetSpec (Events a) d0 i a  -- ^ a list of cohort specifications
  -> CohortApp IO
makeCohortApp name version shape spec = MkCohortApp $ \l -> do
  options <- execParser (makeAppArgs name version)
  let errLog = logStringStderr

  errLog <& "Creating cohort builder..."
  app <- makeCohortBuilder spec

  errLog <& "Reading data from stdin..."
  -- TODO: give error if no contents within some amount of time

  -- let loc = inputToLocation $ input options
  let loc = case l of
        Nothing -> inputToLocation $ input options
        Just x  -> x

  dat <- readData loc

  errLog <& "Bulding cohort..."
  res <- shapeOutput shape (app dat)

  logParseErrors (fst res)

  errLog <& "Encoding cohort(s) output and writing to stdout..."

  return (encode (toJSON (snd res)))

-- | Just run the thing.
runApp :: CohortApp IO -> IO ()
runApp x = C.putStrLn =<< runCohortApp x Nothing

-- | Just run the thing with a set location (e.g for testing).
runAppWithLocation :: Location -> CohortApp IO -> IO B.ByteString
runAppWithLocation l x = runCohortApp x (Just l)
