{-|
Module      : Hasklepias.MakeApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Hasklepias.MakeCohortApp
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

{-| INTERNAL
TODO
-}
data MakeCohort = MakeCohort
  { input  :: Input
  , output :: Output
  }


{-| INTERNAL
TODO
-}
collectBySubject :: [(SubjectID, d)] -> [(SubjectID, [d])]
collectBySubject x = M.toList $ M.fromListWith (++) (fmap (fmap pure) x)

{-| INTERNAL
TODO
-}
mapIntoPop
  :: forall d c a
   . (Ord a, Ord c, Eq d)
  => [(SubjectID, Event d c a)]
  -> Population [Event d c a]
mapIntoPop l = into $ fmap
  (\(id, es) -> into @(Subject [Event d c a]) (into @Text id, sort es)) -- TODO: is there a way to avoid the sort?
  (collectBySubject l)

{-| INTERNAL
TODO
-}
mainOptions :: Parser MakeCohort
mainOptions =
  MakeCohort
    <$> (fileInput <|> s3Input <|> stdInput)
    <*> (fileOutput <|> s3Output <|> stdOutput)

{-| INTERNAL
TODO
-}
makeAppArgs :: String -> String -> ParserInfo MakeCohort
makeAppArgs name version = Options.Applicative.info
  (mainOptions <**> helper)
  (fullDesc <> header (name <> " " <> version))

{-| INTERNAL
Creates a cohort builder function
-}
makeCohortBuilder
  :: ( Show d
     , Eq d
     , Generic d
     , FromJSON d
     , Typeable d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Typeable a
     , Show a
     , IntervalSizeable a b
     , ToJSON d0
     , ShapeCohort d0 i
     , Monad m
     )
  => CohortEvalOptions
  -> CohortMapSpec [Event d c a] d0 i
  -> B.ByteString
  -> m ([LineParseError], CohortMap d0 i)
makeCohortBuilder opts specs x = do
  -- TODO: clean this up
  let dat = parseEventLinesL defaultParseEventLineOption x
  let err          = fst dat
  let doEvaluation = makeCohortSpecsEvaluator opts specs
  let pop          = mapIntoPop $ snd dat
  let res          = doEvaluation pop
  let res2         = (err, ) =<< res

  pure res2



reshapeCohortMap
  :: (Cohort d0 i -> CohortJSON) -> CohortMap d0 i -> CohortMapJSON
reshapeCohortMap g x =
  MkCohortMapJSON $ fromList $ fmap (fmap g) (toList $ into x)

shapeOutput
  :: (Monad m, ShapeCohort d0 i)
  => (Cohort d0 i -> CohortJSON)
  -> m ([LineParseError], CohortMap d0 i)
  -> m ([LineParseError], CohortMapJSON)
shapeOutput shape = fmap (fmap (reshapeCohortMap shape))

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
     , Typeable d
     , Show c
     , Eq c
     , Ord c
     , Typeable c
     , FromJSON c
     , FromJSON a
     , Typeable a
     , Show a
     , IntervalSizeable a b
     , ToJSON d0
     , ShapeCohort d0 i
     )
  => String  -- ^ cohort name
  -> String  -- ^ app version
  -> (Cohort d0 i -> CohortJSON) -- ^ a function which specifies the output shape
  -> CohortMapSpec [Event d c a] d0 i  -- ^ a list of cohort specifications
  -> CohortApp IO
makeCohortApp name version shape spec = MkCohortApp $ \l -> do
  options <- execParser (makeAppArgs name version)
  let errLog = logStringStderr

  errLog <& "Creating cohort builder..."
  -- TODO: wire up ability to change evaluation options. 
  -- For now, set to default.
  let app = makeCohortBuilder defaultCohortEvalOptions spec

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
