{-# LANGUAGE BlockArguments #-}
{-|
Module      : Hasklepias.MakeApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Hasklepias.MakeApp (
   makeCohortApp
) where

import Control.Monad                        ( Monad(return), Functor(fmap) )
import Control.Applicative                  ( Applicative )
import Data.Aeson                           ( encode, FromJSON, ToJSON(..) )
import Data.Bifunctor                       ( Bifunctor(second) )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C      ( putStrLn )
import Data.Function                        ( ($), (.) )
import Data.List                            ( (++) )
import Data.Maybe                           ( Maybe )
import Data.Monoid                          ( Monoid(mconcat) )
import Data.String                          ( String )
import Data.Text                            ( pack, Text )
import Data.Tuple                           ( fst, snd )
import GHC.Show                             ( Show(show) )
import GHC.IO                               ( IO )

import EventData                            ( Events )
import Hasklepias.Cohort
import IntervalAlgebra                      ( IntervalSizeable )

import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Reader                 (MonadReader (..), ReaderT (..))
import Colog                                ( Message
                                            , HasLog(..)
                                            , WithLog
                                            , LogAction(..)
                                            , richMessageAction
                                            , logInfo
                                            , logError
                                            , logStringStdout
                                            , logStringStderr
                                            , logText
                                            , withLog
                                            , logPrint
                                            , logPrintStderr
                                            , (<&)
                                            , (>$)
                                            , log )
import System.Console.CmdArgs               ( Data, Typeable
                                            , cmdArgs, summary, help, (&=) )
import System.Environment                   (getArgs)

-- a stub to add more arguments to later
data MakeCohort = MakeCohort deriving (Show, Data, Typeable)

makeAppArgs ::
     String  -- ^ name of the application
  -> String  -- ^ version of the application 
  -> MakeCohort
makeAppArgs name version = MakeCohort
    {
    } &= help "Pass event data via stdin."
      &= summary (name ++ " " ++ version)

makeCohortBuilder :: (FromJSON a, Show a, IntervalSizeable a b, ToJSON d0, ShapeCohort d0, Monad m) =>
     [CohortSpec (Events a) d0]
  -> m (B.ByteString -> m ([ParseError], [Cohort d0]))
makeCohortBuilder specs =
  return (return . second (\pop -> fmap (`evalCohort` pop) specs) . parsePopulationLines)

reshapeWith :: (Cohort d -> CohortShape shape) 
        -> Cohort d 
        -> (Maybe AttritionInfo, CohortShape shape)
reshapeWith s x = (getAttritionInfo x, s x)

shapeOutput ::  (Monad m, ShapeCohort d0) => (Cohort d0 -> CohortShape shape) 
      -> m ([ParseError], [Cohort d0]) 
      -> m ([ParseError], [(Maybe AttritionInfo, CohortShape shape)]) 
shapeOutput shape = fmap (fmap (fmap (reshapeWith shape)))
  -- fmap (fmap (fmap shape))

-- logging based on example here:
-- https://github.com/kowainik/co-log/blob/main/co-log/tutorials/Main.hs
parseErrorL :: LogAction IO ParseError
parseErrorL = logPrintStderr

logParseErrors :: [ParseError] -> IO ()
logParseErrors x = mconcat $ fmap (parseErrorL <&) x

-- | Make a command line cohort building application.
makeCohortApp :: (FromJSON a, Show a, IntervalSizeable a b
                  , ToJSON d0,
                   ShapeCohort d0) =>
       String  -- ^ cohort name
    -> String  -- ^ app version
    -> (Cohort d0 -> CohortShape shape) -- ^ a function which specifies the output shape
    -> [CohortSpec (Events a) d0]  -- ^ a list of cohort specifications
    -> IO ()
makeCohortApp name version shape spec =
    do
      args <- cmdArgs ( makeAppArgs name version )
      let logger = logStringStdout

      logger <& "Creating cohort builder..."
      app <- makeCohortBuilder spec

      logger <& "Reading data from stdin..."
      -- TODO: give error if no contents within some amount of time
      dat  <- B.getContents

      logger <& "Bulding cohort..."
      res <- shapeOutput shape (app dat)

      logParseErrors (fst res)

      logger <& "Encoding cohort(s) output and writing to stdout..."
      C.putStrLn (encode (fmap (second toJSONCohortShape) (snd res) ))

      logger <& "Cohort build complete!"
