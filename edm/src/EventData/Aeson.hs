{-|
Module      : Functions for Parsing Event data model
Description : Defines FromJSON instances for Events.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EventData.Aeson
  ( parseEventIntLines
  , parseEventDayLines
  ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as B
import           Data.Either                    ( Either(..)
                                                , either
                                                , partitionEithers
                                                )
import           Data.Maybe                     ( fromMaybe, Maybe )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           Data.Vector                    ( (!) )
import           EventData.Context              ( Concept
                                                , Concepts
                                                , Context(..)
                                                , Source
                                                , context
                                                , packConcept
                                                , toConcepts
                                                )
import           EventData.Context.Domain
import           EventData.Core                 ( Event
                                                , event
                                                )
import           IntervalAlgebra                ( Interval
                                                , IntervalSizeable
                                                  ( add
                                                  , diff
                                                  , moment
                                                  )
                                                , beginerval
                                                , parseInterval
                                                )
import           Prelude                        ( ($)
                                                , (<$>)
                                                , (<*>)
                                                , Int
                                                , Ord
                                                , Show
                                                , String
                                                , fmap
                                                , id
                                                , pure
                                                , show
                                                )

instance (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (Interval a) where
  parseJSON = withObject "Time" $ \o -> do
    t <- o .: "time"
    b <- t .: "begin"
    e <- t .:? "end"
    -- In the case that the end is missing, create a moment
    let e2 = fromMaybe (add (moment @a) b) e
    let ei = parseInterval b e2
    case ei of
      Left  e -> fail (show e)
      Right i -> return i

instance FromJSON Domain where
  parseJSON = withObject "Domain" $ \o -> do
    domain :: Text <- o .: "domain"
    case domain of
      "Death"        -> pure $ Death DeathFacts
      "Demographics" -> Demographics <$> o .: "facts"
      "Diagnosis"    -> Diagnosis <$> o .: "facts"
      "Eligibility"  -> Eligibility <$> o .: "facts"
      "Enrollment"   -> Enrollment <$> o .: "facts" 
      "Labs"         -> Labs <$> o .: "facts"
      "Medication"   -> Medication <$> o .: "facts"
      "Procedure"    -> Procedure <$> o .: "facts"
      _              -> pure (UnimplementedDomain ())

instance FromJSON Concept where
  parseJSON c = packConcept <$> parseJSON c

instance FromJSON Concepts where
  parseJSON c = toConcepts <$> parseJSON c

instance FromJSON Source where 

instance FromJSON Context where
  parseJSON = withArray "Context" $ \a -> do
    cpts <- parseJSON (a ! 4)
    fcts <- parseJSON (a ! 5)
    srce <- withObject "source" (.:? "source") (a ! 5)
    return $ Context cpts fcts srce

instance  (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (Event a) where
  parseJSON (Array v) = event <$> parseJSON (v ! 5) <*> parseJSON (Array v)

-- |  Parse @Event Int@ from json lines.
parseEventLines
  :: (FromJSON a, Show a, IntervalSizeable a b)
  => B.ByteString
  -> ([String], [Event a])
parseEventLines l = partitionEithers $ fmap
  (\x ->
    eitherDecode $ B.fromStrict x :: (FromJSON a, Show a, IntervalSizeable a b)
      => Either String (Event a)
  )
  (C.lines $ B.toStrict l)

-- |  Parse @Event Int@ from json lines.
parseEventIntLines
  :: (FromJSON a, Show a, IntervalSizeable a b)
  => B.ByteString
  -> ([String], [Event a])
parseEventIntLines = parseEventLines

-- |  Parse @Event Day@ from json lines.
parseEventDayLines
  :: (FromJSON a, Show a, IntervalSizeable a b)
  => B.ByteString
  -> ([String], [Event a])
parseEventDayLines = parseEventLines
