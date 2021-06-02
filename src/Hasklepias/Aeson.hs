{-|
Module      : Functions for Parsing Hasklepias populations 
Description : Defines FromJSON instances for Hasklepias populations .
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasklepias.Aeson(
      parsePopulationIntLines
    , parsePopulationDayLines
) where

import IntervalAlgebra
import EventData
import EventData.Aeson 
import Hasklepias.Cohort
import Data.Aeson                           ( FromJSON(..)
                                            , eitherDecode
                                            , Value(Array))
import qualified Data.ByteString.Lazy as B  ( fromStrict
                                            , toStrict
                                            , ByteString)
import qualified Data.ByteString.Char8 as C ( lines )
import Prelude                              ( (<$>), (<*>), ($), fmap
                                            , Int, String, Ord, Show)
import Data.Either                          ( Either, rights )
import Data.List                            ( sort, (++) )
import qualified Data.Map.Strict as M       ( toList, fromListWith)
import Data.Vector                          ( (!) )
import Data.Time.Calendar                   ( Day )

newtype SubjectEvent a = MkSubjectEvent (ID, Event a)

subjectEvent :: ID -> Event a -> SubjectEvent a
subjectEvent x y = MkSubjectEvent (x, y)

instance (FromJSON a, Show a, IntervalSizeable a b) => FromJSON (SubjectEvent a) where
    parseJSON (Array v) = subjectEvent <$> 
        parseJSON (v ! 0) <*> (event <$> parseJSON (v ! 5) <*> parseJSON (Array v))

mapIntoPop :: (Ord a) => [SubjectEvent a] -> Population (Events a)
mapIntoPop l = MkPopulation $ 
    fmap (\(id, es) -> MkSubject (id, sort es)) -- TODO: is there a way to avoid the sort
        (M.toList $ M.fromListWith (++) (fmap (\(MkSubjectEvent (id, e)) -> (id, [e])) l ))

-- |  Parse @Event Int@ from json lines.
parsePopulationLines :: 
    (FromJSON a, Show a, IntervalSizeable a b) => 
        B.ByteString -> Population (Events a)
parsePopulationLines l =
    mapIntoPop $ rights $ fmap 
    (\x -> eitherDecode $ B.fromStrict x :: (FromJSON a, Show a, IntervalSizeable a b) => Either String (SubjectEvent a))
        (C.lines $ B.toStrict l)


parsePopulationIntLines :: B.ByteString -> Population (Events Int)
parsePopulationIntLines = parsePopulationLines

-- |  Parse @Event Day@ from json lines.
parsePopulationDayLines :: B.ByteString -> Population (Events Day)
parsePopulationDayLines = parsePopulationLines
