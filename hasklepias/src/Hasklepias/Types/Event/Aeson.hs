{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-|
Module      : Parsing Hasklepias Event Type
Description : Specifies the Event type and related functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Types.Event.Aeson(
      Context(..)
    , Event(..)
    , parseEventIntLines
    , parseEventDayLines
) where

import IntervalAlgebra
import Hasklepias.Types.Context
import Hasklepias.Types.Event
import Data.Aeson
import Data.Time
import Data.Vector ((!))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Either (rights)

-- TODO use parseInterval instead of unsafeInterval
instance FromJSON (Interval Int) where
    parseJSON = withObject "Time" $ \o -> do
        t <- o .: "time"
        b <- t .: "begin"
        e <- t .: "end"
        return (unsafeInterval (b :: Int) (e :: Int))

instance FromJSON (Interval Day) where
    parseJSON = withObject "Time" $ \o -> do
        t <- o .: "time"
        b <- t .: "begin"
        e <- t .: "end"
        return (unsafeInterval (b :: Day) (e :: Day))

instance FromJSON Context where
    parseJSON v = context <$> (parseJSON v)

instance FromJSON (Event Int) where
    parseJSON (Array v) = event <$>
            (parseJSON (v ! 5)) <*>
            (parseJSON (v ! 4))

instance FromJSON (Event Day) where
    parseJSON (Array v) = event <$>
            (parseJSON (v ! 5)) <*>
            (parseJSON (v ! 4))

-- | TODO
-- This function and the event parsing in general needs a lot of work to be 
-- production-ready. But this is good enough for prototyping.
parseEventIntLines :: B.ByteString -> [Event Int]
parseEventIntLines l = 
    rights $ map (\x -> eitherDecode $ B.fromStrict x :: Either String (Event Int)) 
        (C.lines $ B.toStrict l)

parseEventDayLines :: B.ByteString -> [Event Day]
parseEventDayLines l = 
    rights $ map (\x -> eitherDecode $ B.fromStrict x :: Either String (Event Day)) 
        (C.lines $ B.toStrict l)