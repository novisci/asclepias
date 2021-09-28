{-|
Module      : Hasklepias.MakeApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hasklepias.MakeApp
  ( makeCohortApp
  ) where

import           Control.Applicative            ( Applicative )
import           Control.Monad                  ( Functor(fmap)
                                                , Monad(return)
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                , encode
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
import qualified Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8    as C
                                                ( putStrLn )
import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.List                      ( (++) )
import           Data.Map.Strict                ( fromList
                                                , toList
                                                )
import           Data.Maybe                     ( Maybe )
import           Data.Monoid                    ( Monoid(mconcat) )
import           Data.String                    ( String )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           GHC.IO                         ( IO )
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
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , help
                                                , summary
                                                )

-- a stub to add more arguments to later
data MakeCohort = MakeCohort
  deriving (Show, Data, Typeable)

makeAppArgs
  :: String  -- ^ name of the application
  -> String  -- ^ version of the application 
  -> MakeCohort
makeAppArgs name version =
  MakeCohort{} &= help "Pass event data via stdin." &= summary
    (name ++ " " ++ version)

makeCohortBuilder
  :: ( FromJSON a
     , Show a
     , IntervalSizeable a b
     , ToJSON d0
     , ShapeCohort d0
     , Monad m
     )
  => CohortSetSpec (Events a) d0 i a
  -> m (B.ByteString -> m ([ParseError], CohortSet d0))
makeCohortBuilder specs =
  return (return . second (evalCohortSet specs) . parsePopulationLines)

reshapeCohortSet :: (Cohort d0 -> CohortJSON) -> CohortSet d0 -> CohortSetJSON
reshapeCohortSet g x =
  MkCohortSetJSON $ fromList $ fmap (fmap g) (toList $ getCohortSet x)

shapeOutput
  :: (Monad m, ShapeCohort d0)
  => (Cohort d0 -> CohortJSON)
  -> m ([ParseError], CohortSet d0)
  -> m ([ParseError], CohortSetJSON)
shapeOutput shape = fmap (fmap (reshapeCohortSet shape))

-- logging based on example here:
-- https://github.com/kowainik/co-log/blob/main/co-log/tutorials/Main.hs
parseErrorL :: LogAction IO ParseError
parseErrorL = logPrintStderr

logParseErrors :: [ParseError] -> IO ()
logParseErrors x = mconcat $ fmap (parseErrorL <&) x

-- | Make a command line cohort building application.
makeCohortApp
  :: (FromJSON a, Show a, IntervalSizeable a b, ToJSON d0, ShapeCohort d0)
  => String  -- ^ cohort name
  -> String  -- ^ app version
  -> (Cohort d0 -> CohortJSON) -- ^ a function which specifies the output shape
  -> CohortSetSpec (Events a) d0 i a  -- ^ a list of cohort specifications
  -> IO ()
makeCohortApp name version shape spec = do
  args <- cmdArgs (makeAppArgs name version)
  -- let logger = logStringStdout
  let errLog = logStringStderr

  errLog <& "Creating cohort builder..."
  app <- makeCohortBuilder spec

  errLog <& "Reading data from stdin..."
  -- TODO: give error if no contents within some amount of time
  dat <- B.getContents

  errLog <& "Bulding cohort..."
  res <- shapeOutput shape (app dat)

  logParseErrors (fst res)

  errLog <& "Encoding cohort(s) output and writing to stdout..."
  C.putStrLn (encode (toJSON (snd res)))

  errLog <& "Cohort build complete!"

