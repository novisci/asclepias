{-|
Module      : Functions for Parsing Event data model
Description : Defines FromJSON instances for Events.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EventData.Aeson(
      parseEventIntLines
    , parseEventDayLines
) where

import IntervalAlgebra                  ( beginerval
                                        , parseInterval
                                        , Interval
                                        , IntervalSizeable(add, diff, moment) )
import EventData.Context                ( Concepts
                                        , Concept
                                        , Context
                                        , context
                                        , packConcept
                                        , toConcepts )
import EventData                        ( Event, event )
import EventData.Context.Domain
import Data.Aeson
                                        -- ( eitherDecode,
                                        -- (.:), (.:?),
                                        -- withObject,
                                        -- FromJSON(parseJSON),
                                        -- Value(Array) )
import Data.Maybe
import Data.Text (Text)
import Data.Time                        ( Day )
import Data.Vector                      ((!))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Either                      (rights, either)
import Control.Monad 

instance (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (Interval a) where
    parseJSON = withObject "Time" $ \o -> do
        t <- o .: "time"
        b <- t .: "begin"
        e <- t .:? "end" 
        -- In the case that the end is missing, create a moment
        let e2 = maybe (add (moment @a) b) (id) e 
        let ei = parseInterval b e2
        case ei of
            Left e  -> fail e
            Right i -> return i

instance FromJSON Domain where
    parseJSON = withObject "Domain" $ \o -> do
        domain :: Text <- o .: "domain"
        case domain of
            "Demographics" -> Demographics <$> o .: "facts"
            _              -> pure (UnimplementedDomain ())

instance FromJSON Concept where
    parseJSON c = packConcept <$> parseJSON  c

instance FromJSON Concepts where
    parseJSON c = toConcepts <$> parseJSON c

instance FromJSON Context where
    parseJSON (Array v) = context <$>
        parseJSON (v ! 5) <*>
        parseJSON (v ! 4)
    -- parseJSON v = context <$> parseJSON v

instance  (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (Event a) where
    parseJSON (Array v) = event <$> parseJSON (v ! 5) <*> parseJSON (Array v)
    -- parseJSON (Array v) = event <$>
    --         parseJSON (v ! 5) <*>
    --         parseJSON (v ! 4)

-- |  Parse @Event Int@ from json lines.
parseEventLines :: (FromJSON a, Show a, IntervalSizeable a b) => B.ByteString -> [Event a]
parseEventLines l =
    rights $ map 
    (\x -> eitherDecode $ B.fromStrict x :: (FromJSON a, Show a, IntervalSizeable a b) =>  Either String (Event a))
        (C.lines $ B.toStrict l)


parseEventIntLines :: B.ByteString -> [Event Int]
parseEventIntLines = parseEventLines

-- |  Parse @Event Day@ from json lines.
parseEventDayLines :: B.ByteString -> [Event Day]
parseEventDayLines = parseEventLines
