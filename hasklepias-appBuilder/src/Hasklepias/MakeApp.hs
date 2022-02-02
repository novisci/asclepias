{-|
Module      : Hasklepias.MakeApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}


module Hasklepias.MakeApp
  ( CohortApp(..)
  , makeCohortApp
  , shapeOutput
  , runApp
  , runAppWithLocation
  ) where


import           Cohort
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                , decode
                                                , encode
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.List                      ( sort )
import           Data.Map.Strict                ( fromList
                                                , toList
                                                )
import           Data.Monoid                    ( Monoid(mconcat) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           EventDataTheory         hiding ( (<|>) )
import           GHC.Num                        ( Natural )
import           IntervalAlgebra                ( IntervalSizeable )

import           Colog.Core                     ( (<&)
                                                , HasLog(..)
                                                , LogAction(..)
                                                , logPrint
                                                , logPrintStderr
                                                , logStringStderr
                                                , logStringStdout
                                                )
import qualified Data.ByteString.Char8         as CH
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C
                                                ( lines
                                                , putStrLn
                                                , toStrict
                                                )
import qualified Data.Map.Strict               as M
                                                ( fromListWith
                                                , toList
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           GHC.Generics                   ( Generic )
import           Hasklepias.AppUtilities
import           Options.Applicative
import           Type.Reflection                ( Typeable )
import           Witch                          ( into )

data MakeCohort = MakeCohort
  { input  :: Input
  , output :: Output
  }

mapIntoPop
  :: (Ord a, Ord c, Eq d)
  => [(SubjectID, Event d c a)]
  -> Population [Event d c a]
mapIntoPop l = MkPopulation $ fmap
  (\(id, es) -> MkSubject (into @Text id, sort es)) -- TODO: is there a way to avoid the sort?
  (M.toList $ M.fromListWith (++) (fmap (fmap pure) l))

mainOptions :: Parser MakeCohort
mainOptions =
  MakeCohort
    <$> (fileInput <|> s3Input <|> stdInput)
    <*> (fileOutput <|> s3Output <|> stdOutput)

makeAppArgs :: String -> String -> ParserInfo MakeCohort
makeAppArgs name version = Options.Applicative.info
  (mainOptions <**> helper)
  (fullDesc <> header (name <> " " <> version))

-- | Creates a cohort builder function
makeCohortBuilder
  :: ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     , ToJSON d0
     , ShapeCohort d0
     , Monad m
     )
  => CohortSetSpec [Event d c a] d0 i a
  -> m (B.ByteString -> m ([LineParseError], CohortSet d0))
makeCohortBuilder specs =
  pure (pure . second (evalCohortSet specs . mapIntoPop) . parseEventLinesL)

reshapeCohortSet :: (Cohort d0 -> CohortJSON) -> CohortSet d0 -> CohortSetJSON
reshapeCohortSet g x =
  MkCohortSetJSON $ fromList $ fmap (fmap g) (toList $ getCohortSet x)

shapeOutput
  :: (Monad m, ShapeCohort d0)
  => (Cohort d0 -> CohortJSON)
  -> m ([LineParseError], CohortSet d0)
  -> m ([LineParseError], CohortSetJSON)
shapeOutput shape = fmap (fmap (reshapeCohortSet shape))

-- logging based on example here:
-- https://github.com/kowainik/co-log/blob/main/co-log/tutorials/Main.hs
parseErrorL :: LogAction IO LineParseError
parseErrorL = logPrintStderr

logParseErrors :: [LineParseError] -> IO ()
logParseErrors x = mconcat $ fmap (parseErrorL <&) x

-- | Type containing the cohort app
newtype CohortApp m = MkCohortApp { runCohortApp :: Maybe Location -> m B.ByteString }

-- | Make a command line cohort building application.
makeCohortApp
  :: ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Show a
     , IntervalSizeable a b
     , ToJSON d0
     , ShapeCohort d0
     )
  => String  -- ^ cohort name
  -> String  -- ^ app version
  -> (Cohort d0 -> CohortJSON) -- ^ a function which specifies the output shape
  -> CohortSetSpec [Event d c a] d0 i a  -- ^ a list of cohort specifications
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

  pure (encode (toJSON (snd res)))

-- | Just run the thing.
runApp :: CohortApp IO -> IO ()
runApp x = do
  options <- execParser (makeAppArgs "" "")
  writeData (outputToLocation (output options)) =<< runCohortApp x Nothing

-- | Just run the thing with a set location (e.g for testing).
runAppWithLocation :: Location -> CohortApp IO -> IO B.ByteString
runAppWithLocation l x = runCohortApp x (Just l)
