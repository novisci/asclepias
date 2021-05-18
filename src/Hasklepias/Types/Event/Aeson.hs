{-|
Module      : Functions for Parsing Hasklepias Event data
Description : Defines FromJSON instances for Events.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Hasklepias.Types.Event.Aeson(
      parseEventIntLines
    , parseEventDayLines
) where

import IntervalAlgebra
    ( beginerval, Interval, IntervalSizeable(diff) )
import Hasklepias.Types.Context
    ( Concepts, Concept, Context, context, packConcept, toConcepts )
import Hasklepias.Types.Event ( Event, event )
import Data.Aeson
    ( eitherDecode,
      (.:),
      withObject,
      FromJSON(parseJSON),
      Value(Array) )
import Data.Time ( Day )
import Data.Vector ((!))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Either (rights, fromRight)

instance FromJSON (Interval Int) where
    parseJSON = withObject "Time" $ \o -> do
        t <- o .: "time"
        b <- t .: "begin"
        e <- t .: "end"
        return $ beginerval (diff e b) b
        --(parseInterval (b :: Day) (e :: Day))

instance FromJSON (Interval Day) where
    parseJSON = withObject "Time" $ \o -> do
        t <- o .: "time"
        b <- t .: "begin"
        e <- t .: "end"
        return  $ beginerval (diff e b) b 
        --(parseInterval (b :: Day) (e :: Day))

instance FromJSON Concept where
    parseJSON c = packConcept <$> parseJSON  c

instance FromJSON Concepts where
    parseJSON c = toConcepts <$> parseJSON c

instance FromJSON Context where
    parseJSON v = context <$> parseJSON v

instance FromJSON (Event Int) where
    parseJSON (Array v) = event <$>
            parseJSON (v ! 5) <*>
            parseJSON (v ! 4)

instance FromJSON (Event Day) where
    parseJSON (Array v) = event <$>
            parseJSON (v ! 5) <*>
            parseJSON (v ! 4)

-- |  Parse @Event Int@ from json lines.
-- 
-- This function and the event parsing in general needs a lot of work to be 
-- production-ready. But this is good enough for prototyping.
parseEventIntLines :: B.ByteString -> [Event Int]
parseEventIntLines l =
    rights $ map (\x -> eitherDecode $ B.fromStrict x :: Either String (Event Int))
        (C.lines $ B.toStrict l)

-- |  Parse @Event Day@ from json lines.
parseEventDayLines :: B.ByteString -> [Event Day]
parseEventDayLines l =
    rights $ map (\x -> eitherDecode $ B.fromStrict x :: Either String (Event Day))
        (C.lines $ B.toStrict l)